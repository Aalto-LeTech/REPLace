package fi.aalto.cs.replace.actions

import com.intellij.execution.configurations.*
import com.intellij.execution.runners.ExecutionEnvironment
import com.intellij.execution.{Executor, RunManagerEx, RunnerAndConfigurationSettings}
import com.intellij.notification.{Notification, NotificationType, Notifications}
import com.intellij.openapi.actionSystem.{AnActionEvent, CommonDataKeys}
import com.intellij.openapi.application.PathManager
import com.intellij.openapi.module.Module
import com.intellij.openapi.project.Project
import com.intellij.openapi.diagnostic.Logger
import com.intellij.openapi.roots.ModuleRootManager
import com.intellij.openapi.ui.popup.JBPopupFactory
import com.intellij.util.lang.JavaVersion
import fi.aalto.cs.replace.utils.MyBundle.*
import fi.aalto.cs.replace.utils.ModuleUtils
import fi.aalto.cs.replace.{Repl, ReplConsoleBuilder}
import org.jetbrains.plugins.scala.ScalaVersion
import org.jetbrains.plugins.scala.console.actions.RunConsoleAction
import org.jetbrains.plugins.scala.console.configuration.ScalaConsoleRunConfiguration
import org.jetbrains.plugins.scala.project.{ModuleExt, ProjectExt, ScalaLanguageLevel}

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.StandardCopyOption.{ATOMIC_MOVE, REPLACE_EXISTING}
import java.nio.file.{Files, Path}
import javax.swing.ListSelectionModel
import scala.jdk.CollectionConverters.*
import scala.util.control.NonFatal

/** The Scala plugin's RunConsoleAction, adjusted to A+ requirements. */
class ReplAction extends RunConsoleAction:

  override def update(e: AnActionEvent): Unit =
    val project = e.getProject
    e.getPresentation.setEnabledAndVisible(
      project != null && !project.isDisposed && project.hasScala
    )

  override def actionPerformed(e: AnActionEvent): Unit =
    val dataContext = e.getDataContext
    Option(CommonDataKeys.PROJECT.getData(dataContext)).foreach { project =>
      ModuleUtils.getScalaReplModule(project, dataContext, isEligible) match
        case Some(module) => runReplFor(project, module)
        case None         =>
          // No rule picked a module out, so the project either has none or has several.
          ModuleUtils.eligibleModules(project, isEligible) match
            case Nil => notifyNoEligibleModule(project)
            case candidates =>
              chooseModule(candidates, project)(module => runReplFor(project, module))
    }

  /** The eligibility every step of the lookup goes by. */
  private[replace] def isEligible(module: Module): Boolean = module.hasScala

  /** Asks which of several equally good modules to start the REPL for. */
  private[replace] def chooseModule(candidates: List[Module], project: Project)(
      onChosen: Module => Unit
  ): Unit =
    // Module names are unique within a project, so they identify the choice on their own.
    val byName = candidates.map(module => module.getName -> module).toMap
    JBPopupFactory.getInstance
      .createPopupChooserBuilder(candidates.map(_.getName).asJava)
      .setTitle(message("ui.repl.chooser.title"))
      .setSelectionMode(ListSelectionModel.SINGLE_SELECTION)
      .setItemChosenCallback(name => byName.get(name).foreach(onChosen))
      .createPopup()
      .showCenteredInCurrentWindow(project)

  private def runReplFor(project: Project, module: Module): Unit =
    val runManagerEx = RunManagerEx.getInstanceEx(project)
    val setting = runManagerEx.createConfiguration(
      message("ui.repl.console.scala.repl"),
      configurationFactory
    )
    // The one case is total: `configurationFactory` builds nothing but a ReplConfiguration.
    setting.getConfiguration match
      case configuration: ReplAction.ReplConfiguration => configuration.configureFor(module)
    launch(setting, runManagerEx, project)

  /** The launch itself, so a test can see what was configured without starting a REPL JVM. */
  private[replace] def launch(
      setting: RunnerAndConfigurationSettings,
      runManagerEx: RunManagerEx,
      project: Project
  ): Unit =
    RunConsoleAction.runExisting(setting, runManagerEx, project)

  private def notifyNoEligibleModule(project: Project): Unit =
    Notifications.Bus.notify(
      new Notification(
        Repl.notificationGroup,
        message("ui.repl.notification.notFound.title"),
        message("ui.repl.notification.notFound.message"),
        NotificationType.WARNING
      ),
      project
    )

  private[replace] lazy val configurationFactory: ReplConfigurationFactory =
    new ReplConfigurationFactory

  private[replace] class ReplConfigurationFactory
      extends ConfigurationFactory(getMyConfigurationType):
    override def createTemplateConfiguration(project: Project): RunConfiguration =
      new ReplAction.ReplConfiguration(project, this, message("ui.repl.console.scala.repl"))

    override def getId: String = ReplAction.configurationFactoryId

/** The REPL configuration, its factory id and the options it is started with.
  *
  * The console configuration API it reshapes is experimental and not meant to be extended, so the
  * unstable-API inspection is suppressed once for the whole object.
  */
//noinspection UnstableApiUsage
object ReplAction:
  /** Persisted inside saved run configurations; must be stable and never localized. */
  private[replace] val configurationFactoryId = "Enhanced Scala REPL"

  /** Without native access the Scala 3 REPL prints a JVM warning on startup (issue #9). */
  private val nativeAccessOption = "--enable-native-access=ALL-UNNAMED"

  /** Silences the JDK 24+ sun.misc.Unsafe warning, and only JDK 23+ recognizes the option. */
  private val unsafeMemoryAccessOption = "--sun-misc-unsafe-memory-access=allow"

  /** The REPL gets pipes rather than a pty, so this skips JLine's failed probe and its warning. */
  private val dumbTerminalOption = "-Dorg.jline.terminal.dumb=true"

  /** The Scala 3.8+ REPL re-defines loaded classes inside its own classloader for interrupt
    * support, which lands the macOS-only `com.apple.*` in the unnamed module and kills every Swing
    * use. `local` keeps interruption for REPL-typed code and normal delegation for the rest.
    */
  private val interruptInstrumentationOption = "-Xrepl-interrupt-instrumentation local"

  /** JLine warns that dotty's REPL parser does not implement CompletingParsedLine. Only JLine's
    * unused line editor is concerned and Scala offers no way to turn it off, so the `org.jline`
    * logger is silenced through a java.util.logging configuration file.
    */
  private val jlineLoggingConfig =
    "handlers=java.util.logging.ConsoleHandler\n.level=INFO\norg.jline.level=OFF\n"

  private val logger = Logger.getInstance(classOf[ReplAction])

  private[replace] class ReplConfiguration(
      project: Project,
      configurationFactory: ConfigurationFactory,
      name: String
  ) extends ScalaConsoleRunConfiguration(project, configurationFactory, name):

    /** Points a freshly created configuration at the module the REPL is started for. */
    def configureFor(module: Module): Unit =
      workingDirectory = ModuleUtils.getModuleDirectory(module)
      setModule(module)
      setName(message("ui.repl.console.name", module.getName))
      javaOptions = javaOptionsFor(module).mkString(" ")
      myConsoleArgs = consoleArgsFor(module)

    override def getState(executor: Executor, env: ExecutionEnvironment): RunProfileState =
      val state = super.getState(executor, env).asInstanceOf[JavaCommandLineState]
      Option(getConfigurationModule.getModule)
        .foreach(module => state.setConsoleBuilder(new ReplConsoleBuilder(module)))
      state

  private def javaOptionsFor(module: Module): List[String] =
    val unsafeMemoryAccess = Option.when(
      supportsUnsafeMemoryAccessOption(moduleJdkVersion(module))
    )(unsafeMemoryAccessOption)
    List(nativeAccessOption, dumbTerminalOption) ++ unsafeMemoryAccess ++ quietJLineLoggingOption()

  /** Writes the logging configuration into the IDE's per-user temporary directory, unlike the
    * shared `java.io.tmpdir` where the first user to start a REPL would own the file, and returns
    * the option pointing at it. A failure only drops the option.
    */
  private[replace] def quietJLineLoggingOption(
      directory: String = PathManager.getTempPath
  ): Option[String] =
    try
      val config = Path.of(directory, "replace-jline-logging.properties")
      Files.createDirectories(config.getParent)
      // Written beside and moved into place, so the JVM of a REPL starting at the same moment
      // never reads a half-written file.
      val fresh = Files.createTempFile(config.getParent, "replace-jline-logging", ".tmp")
      try
        Files.write(fresh, jlineLoggingConfig.getBytes(UTF_8))
        Files.move(fresh, config, REPLACE_EXISTING, ATOMIC_MOVE)
      finally Files.deleteIfExists(fresh)
      // javaOptions reach the JVM through ParametersList.addParametersString, which splits on
      // whitespace outside double quotes and drops those quotes again.
      val path  = config.toString
      val value = if path.exists(_.isWhitespace) then s"\"$path\"" else path
      Some(s"-Djava.util.logging.config.file=$value")
    catch
      case NonFatal(error) =>
        logger.warn("Could not write the REPL logging configuration", error)
        None

  private def consoleArgsFor(module: Module): String =
    // Scala < 3.8 would print a "bad option was ignored" warning for it on startup.
    val instrumentation = Option.when(
      supportsInterruptInstrumentationOption(module.scalaMinorVersion)
    )(interruptInstrumentationOption)
    val initialCommands =
      if ModuleUtils.isScala3Module(module) then None
      else ModuleUtils.initialCommandsFile(module).map(path => s"-i ${path.getFileName}")
    // `-i` comes last, because Scala 2's multi-value loadfiles option keeps consuming arguments
    // until one starts with a dash and would take a .repl-arguments token as another file.
    val arguments = List("-usejavacp") ++ instrumentation ++
      List(Repl.additionalArguments(module.getProject)) ++ initialCommands
    arguments.filter(_.nonEmpty).mkString(" ")

  private def moduleJdkVersion(module: Module): Option[String] =
    Option(ModuleRootManager.getInstance(module).getSdk)
      .flatMap(sdk => Option(sdk.getVersionString))

  private[replace] def supportsUnsafeMemoryAccessOption(jdkVersion: Option[String]): Boolean =
    jdkVersion.flatMap(version => Option(JavaVersion.tryParse(version))).exists(_.feature >= 23)

  private[replace] def supportsInterruptInstrumentationOption(
      scalaVersion: Option[ScalaVersion]
  ): Boolean =
    scalaVersion.exists(_.languageLevel >= ScalaLanguageLevel.Scala_3_8)
