package fi.aalto.cs.replace.actions

import com.intellij.execution.configurations.*
import com.intellij.execution.runners.ExecutionEnvironment
import com.intellij.execution.{Executor, RunManagerEx}
import com.intellij.notification.{Notification, NotificationType, Notifications}
import com.intellij.openapi.actionSystem.{AnActionEvent, CommonDataKeys}
import com.intellij.openapi.module.Module
import com.intellij.openapi.project.Project
import com.intellij.openapi.diagnostic.Logger
import com.intellij.openapi.roots.ModuleRootManager
import com.intellij.util.lang.JavaVersion
import fi.aalto.cs.replace.utils.MyBundle.*
import fi.aalto.cs.replace.utils.ModuleUtils
import fi.aalto.cs.replace.{Repl, ReplConsoleBuilder}
import org.jetbrains.plugins.scala.console.actions.RunConsoleAction
import org.jetbrains.plugins.scala.console.configuration.ScalaConsoleRunConfiguration
import org.jetbrains.plugins.scala.project.ProjectExt

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}
import scala.util.control.NonFatal

/** Custom class that adjusts Scala Plugin's own RunConsoleAction with A+ requirements.
  *
  * Reshaping the Scala console's run configuration is this plugin's entire purpose, and the Scala
  * plugin exposes no stable equivalent: `ScalaConsoleRunConfiguration` is `@ApiStatus.Experimental`
  * and `ConfigurationFactory`'s constructor is `@ApiStatus.Internal`. The unstable-API inspection
  * is therefore suppressed once for the file instead of at every touch point.
  */
//noinspection UnstableApiUsage
class ReplAction extends RunConsoleAction:

  override def update(e: AnActionEvent): Unit =
    Option(e.getProject).foreach(project =>
      e.getPresentation.setEnabledAndVisible(project.hasScala)
    )

  override def actionPerformed(e: AnActionEvent): Unit =
    val dataContext = e.getDataContext
    Option(CommonDataKeys.PROJECT.getData(dataContext)).foreach { project =>
      ModuleUtils.getScalaReplModule(project, dataContext) match
        case Some(module) =>
          val runManagerEx = RunManagerEx.getInstanceEx(project)
          val setting = runManagerEx.createConfiguration(
            message("ui.repl.console.scala.repl"),
            configurationFactory
          )
          val configuration = setting.getConfiguration.asInstanceOf[ScalaConsoleRunConfiguration]
          ReplAction.configureForModule(configuration, module)
          RunConsoleAction.runExisting(setting, runManagerEx, project)
        case None =>
          Notifications.Bus.notify(
            Notification(
              "REPLace",
              message("ui.repl.notification.notFound.title"),
              message("ui.repl.notification.notFound.message"),
              NotificationType.WARNING
            )
          )
          super.actionPerformed(e) // Delegate to the original Scala Plugin REPL
    }

  private[replace] lazy val configurationFactory: ReplConfigurationFactory =
    new ReplConfigurationFactory

  private[replace] class ReplConfigurationFactory
      extends ConfigurationFactory(getMyConfigurationType):
    override def createTemplateConfiguration(project: Project): ScalaConsoleRunConfiguration =
      new ReplConfiguration(project, this, message("ui.repl.console.scala.repl"))

    override def getId: String = ReplAction.configurationFactoryId

    private class ReplConfiguration(
        project: Project,
        configurationFactory: ConfigurationFactory,
        name: String
    ) extends ScalaConsoleRunConfiguration(project, configurationFactory, name):

      override def getState(executor: Executor, env: ExecutionEnvironment): RunProfileState =
        val state = super.getState(executor, env).asInstanceOf[JavaCommandLineState]
        Option(getConfigurationModule.getModule)
          .foreach(module => state.setConsoleBuilder(new ReplConsoleBuilder(module)))
        state

//noinspection UnstableApiUsage
object ReplAction:
  /** Persisted inside saved run configurations; must be stable and never localized. */
  private[replace] val configurationFactoryId = "Enhanced Scala REPL"

  /** Without native access the Scala 3 REPL prints a JVM warning on startup (issue #9). */
  private val nativeAccessOption = "--enable-native-access=ALL-UNNAMED"

  /** Scala's lazy vals use sun.misc.Unsafe, which JDK 24+ warns about on REPL startup. The
    * silencing option exists since JDK 23; older JDKs would refuse to start with it.
    */
  private val unsafeMemoryAccessOption = "--sun-misc-unsafe-memory-access=allow"

  /** The console gives the REPL pipes rather than a pty, so JLine falls back to a dumb terminal.
    * Saying so up front skips the failed attempt and the warning it logs.
    */
  private val dumbTerminalOption = "-Dorg.jline.terminal.dumb=true"

  /** JLine also warns that dotty's own REPL parser does not implement its CompletingParsedLine
    * interface. That only concerns JLine's line editor, which is unused here because input arrives
    * on stdin and completion comes from the IDE, and Scala offers no option to turn it off. The
    * `org.jline` logger is therefore silenced through a java.util.logging configuration file.
    */
  private val jlineLoggingConfig =
    "handlers=java.util.logging.ConsoleHandler\n.level=INFO\norg.jline.level=OFF\n"

  private val logger = Logger.getInstance(classOf[ReplAction])

  private[replace] def configureForModule(
      configuration: ScalaConsoleRunConfiguration,
      module: Module
  ): Unit =
    val scala3 = ModuleUtils.isScala3Module(module)

    configuration.workingDirectory = ModuleUtils.getModuleDirectory(module)
    configuration.setModule(module)
    configuration.setName(message("ui.repl.console.name", module.getName))
    configuration.javaOptions = appendOptions(configuration.javaOptions, javaOptionsFor(module))
    configuration.myConsoleArgs = consoleArgsFor(module, scala3)

  private def javaOptionsFor(module: Module): List[String] =
    val unsafeMemoryAccess = Option.when(
      supportsUnsafeMemoryAccessOption(moduleJdkVersion(module))
    )(unsafeMemoryAccessOption)
    nativeAccessOption :: dumbTerminalOption ::
      unsafeMemoryAccess.toList ::: quietJLineLoggingOption.toList

  /** Writes the logging configuration next to the other temporary files and returns the option
    * pointing at it. A REPL that cannot be made quiet is still perfectly usable, so a failure here
    * only drops the option.
    */
  private[replace] def quietJLineLoggingOption: Option[String] =
    try
      val config = Path.of(System.getProperty("java.io.tmpdir"), "replace-jline-logging.properties")
      Files.writeString(config, jlineLoggingConfig, UTF_8)
      Some(s"-Djava.util.logging.config.file=$config")
    catch
      case NonFatal(error) =>
        logger.warn("Could not write the REPL logging configuration", error)
        None

  private def consoleArgsFor(module: Module, scala3: Boolean): String =
    val args = "-usejavacp " + Repl.additionalArguments(module.getProject)
    // Scala 3 no longer has an option for preloading REPL commands, so there's no point in adding
    // this command-line switch anymore. Instead, we use an alternative method of preloading
    // commands by using ScalaExecutor.
    if scala3 then args
    else
      ModuleUtils.createInitialReplCommandsFile(module)
      if ModuleUtils.initialReplCommandsFileExists(module) then
        s"$args -i ${Repl.initialCommandsFileName}"
      else args

  private def moduleJdkVersion(module: Module): Option[String] =
    Option(ModuleRootManager.getInstance(module).getSdk)
      .flatMap(sdk => Option(sdk.getVersionString))

  private[replace] def supportsUnsafeMemoryAccessOption(jdkVersion: Option[String]): Boolean =
    jdkVersion.flatMap(version => Option(JavaVersion.tryParse(version))).exists(_.feature >= 23)

  private def appendOptions(existingOptions: String | Null, options: List[String]): String =
    options.foldLeft(Option(existingOptions).getOrElse("").trim)(appendOption)

  private def appendOption(existingOptions: String, option: String): String =
    if existingOptions.isEmpty then option
    else if existingOptions.contains(option) then existingOptions
    else s"$existingOptions $option"
