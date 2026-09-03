package fi.aalto.cs.replace

import com.intellij.execution.{RunManager, RunManagerEx, RunnerAndConfigurationSettings}
import com.intellij.notification.{Notification, NotificationsManager}
import com.intellij.openapi.actionSystem.impl.SimpleDataContext
import com.intellij.openapi.actionSystem.{AnActionEvent, CommonDataKeys}
import com.intellij.openapi.module.Module
import com.intellij.openapi.project.Project
import com.intellij.testFramework.TestActionEvent
import com.intellij.util.execution.ParametersListUtil
import fi.aalto.cs.replace.actions.ReplAction
import fi.aalto.cs.replace.utils.ModuleUtils
import fi.aalto.cs.replace.utils.MyBundle.message
import org.jetbrains.plugins.scala.ScalaVersion
import org.junit.Assert.{assertEquals, assertFalse, assertTrue, fail}
import org.junit.jupiter.api.Test

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*

/** The [[ReplAction]] companion says why the experimental console-configuration API is used. */
//noinspection UnstableApiUsage
class ReplActionTest extends ReplPlatformTestBase:

  private def newConfiguration: ReplAction.ReplConfiguration =
    new ReplAction().configurationFactory
      .createTemplateConfiguration(getProject)
      .asInstanceOf[ReplAction.ReplConfiguration]

  private def configuredForModule: ReplAction.ReplConfiguration =
    val configuration = newConfiguration
    configuration.configureFor(getModule)
    configuration

  /** An action event that identifies the project and nothing else. */
  private def projectOnlyEvent: AnActionEvent =
    TestActionEvent.createTestEvent(
      SimpleDataContext.getSimpleContext(CommonDataKeys.PROJECT, getProject)
    )

  /** The "module not found" reports the platform still holds for this project. */
  private def notFoundNotifications: Seq[Notification] =
    NotificationsManager.getNotificationsManager
      .getNotificationsOfType(classOf[Notification], getProject)
      .filter(_.getTitle == message("ui.repl.notification.notFound.title"))
      .toSeq

  @Test
  def testConfigurationFactoryIdIsStableAcrossLocales(): Unit =
    // The factory ID is persisted in run configurations and must never be localized.
    assertEquals("Enhanced Scala REPL", new ReplAction().configurationFactory.getId)

  @Test
  def testAProjectWithoutAnEligibleModuleIsOnlyReported(): Unit =
    // The light test module has no Scala SDK, so nothing is eligible. The report is the whole
    // response: the stock action used to follow it with a REPL bound to no module at all
    // (issue #10). The found-module branch is pinned through `configureFor`.
    val reportsBefore        = notFoundNotifications.size
    val configurationsBefore = RunManager.getInstance(getProject).getAllSettings.size

    new ReplAction().actionPerformed(projectOnlyEvent)

    assertEquals(
      "this Enter must be the one that told the user why no REPL was started for a module",
      reportsBefore + 1,
      notFoundNotifications.size
    )
    assertEquals(
      "nothing may be launched",
      configurationsBefore,
      RunManager.getInstance(getProject).getAllSettings.size
    )

  @Test
  def testSeveralEligibleModulesAreOfferedForChoosing(): Unit =
    // Two candidates and nothing selected: the user picks, and nothing starts until they have.
    val other                = createSecondModule()
    val configurationsBefore = RunManager.getInstance(getProject).getAllSettings.size
    var offered              = List.empty[Module]
    val action = new ReplAction:
      override private[replace] def isEligible(module: Module): Boolean = true
      override private[replace] def chooseModule(
          candidates: List[Module],
          project: Project
      )(onChosen: Module => Unit): Unit = offered = candidates

    action.actionPerformed(projectOnlyEvent)

    assertEquals(List(getModule, other).sortBy(_.getName), offered)
    assertEquals(
      "the choice must come before the launch",
      configurationsBefore,
      RunManager.getInstance(getProject).getAllSettings.size
    )

  @Test
  def testTheReplStartsForTheModuleTheChooserPicked(): Unit =
    // The chooser answers through a callback, and a launch that ignored it would start the REPL
    // for a module the user did not pick.
    val other       = createSecondModule()
    var launchedFor = Option.empty[Module]
    val action = new ReplAction:
      override private[replace] def isEligible(module: Module): Boolean = true
      override private[replace] def chooseModule(
          candidates: List[Module],
          project: Project
      )(onChosen: Module => Unit): Unit = onChosen(other)
      override private[replace] def launch(
          setting: RunnerAndConfigurationSettings,
          runManagerEx: RunManagerEx,
          project: Project
      ): Unit =
        launchedFor = setting.getConfiguration match
          case configuration: ReplAction.ReplConfiguration =>
            Option(configuration.getConfigurationModule.getModule)
          case _ => None

    action.actionPerformed(projectOnlyEvent)

    assertEquals(Some(other), launchedFor)

  @Test
  def testConfigurationIsNamedAfterItsModule(): Unit =
    assertEquals(message("ui.repl.console.name", getModule.getName), configuredForModule.getName)

  @Test
  def testConfigurationSetsTheReplJvmOptions(): Unit =
    val options = configuredForModule.javaOptions
    // Native access silences a JVM warning (issue #9); the dumb-terminal property skips JLine's
    // failed pty probe.
    assertTrue(options, options.contains("--enable-native-access=ALL-UNNAMED"))
    assertTrue(options, options.contains("-Dorg.jline.terminal.dumb=true"))
    // The fixture module has no SDK, and an older JDK refuses to start with the option.
    assertFalse(options, options.contains("--sun-misc-unsafe-memory-access"))

  @Test
  def testConfigurationSetsTheReplConsoleArguments(): Unit =
    val args = configuredForModule.myConsoleArgs
    assertTrue(args, args.contains("-usejavacp"))
    // The fixture module has no Scala SDK, and a REPL below 3.8 warns about the option.
    assertFalse(args, args.contains("-Xrepl-interrupt-instrumentation"))

  @Test
  def testScala2ConfigurationPreloadsTheInitialCommandsFile(): Unit =
    // Same module either way, so the argument tracks the file rather than the fixture: a Scala 2
    // REPL is handed the file it has, and given nothing to preload when there is none.
    val withoutFile = configuredForModule.myConsoleArgs
    assertFalse(withoutFile, withoutFile.contains("-i .repl-commands"))

    val commandsFile = Path.of(ModuleUtils.getModuleDirectory(getModule), ".repl-commands")
    Files.writeString(commandsFile, "def answer =\n  42\n")

    try
      val args = configuredForModule.myConsoleArgs
      assertTrue(args, args.contains("-i .repl-commands"))
    finally Files.deleteIfExists(commandsFile)

  @Test
  def testProjectSpecificReplArgumentsReachTheConsoleArguments(): Unit =
    // A course project ships extra REPL arguments in .idea/.repl-arguments; they must end up on
    // the REPL's own command line, next to the ones this plugin adds itself.
    val ideaDirectory = Path.of(getProject.getBasePath, ".idea")
    Files.createDirectories(ideaDirectory)
    val arguments = ideaDirectory.resolve(".repl-arguments")
    Files.writeString(arguments, "-Yexplicit-nulls\n")

    try
      val args = configuredForModule.myConsoleArgs
      assertTrue(args, args.contains("-usejavacp"))
      assertTrue(args, args.contains("-Yexplicit-nulls"))
    finally Files.deleteIfExists(arguments)

  @Test
  def testTheCommandsFileArgumentComesAfterTheProjectsOwnArguments(): Unit =
    // Scala 2's -i keeps consuming arguments until one starts with a dash, so a .repl-arguments
    // token behind it would be loaded as a second file instead of reaching the REPL as an option.
    val ideaDirectory = Path.of(getProject.getBasePath, ".idea")
    Files.createDirectories(ideaDirectory)
    val arguments    = ideaDirectory.resolve(".repl-arguments")
    val commandsFile = Path.of(ModuleUtils.getModuleDirectory(getModule), ".repl-commands")
    Files.writeString(arguments, "-Yexplicit-nulls\n")
    Files.writeString(commandsFile, "import o1.*\n")

    try
      val args = configuredForModule.myConsoleArgs
      assertTrue(args, args.contains("-Yexplicit-nulls"))
      assertTrue(args, args.endsWith("-i .repl-commands"))
    finally
      Files.deleteIfExists(arguments)
      Files.deleteIfExists(commandsFile)

  @Test
  def testTheUnsafeMemoryAccessOptionIsGatedOnTheJdkVersion(): Unit =
    // The option that silences the JDK 24+ sun.misc.Unsafe warning exists only since JDK 23, and
    // an older JDK would refuse to start with it. An unreadable version counts as older.
    val cases = List(
      Some("openjdk version \"25.0.3\"") -> true,
      Some("java version \"23\"")        -> true,
      Some("openjdk version \"17.0.9\"") -> false,
      Some("version 11.0.2")             -> false,
      Some("not a version")              -> false,
      None                               -> false
    )
    cases.foreach { (version, expected) =>
      assertEquals(
        s"$version",
        expected,
        ReplAction.supportsUnsafeMemoryAccessOption(version)
      )
    }

  @Test
  def testTheInterruptInstrumentationOptionIsGatedOnTheScalaVersion(): Unit =
    // "local" instrumentation is what keeps the 3.8+ REPL from breaking Swing on macOS (see
    // ReplAction), and older REPLs would print "bad option ... was ignored" for it.
    val cases =
      List("3.8.4" -> true, "3.9.0" -> true, "3.7.1" -> false, "3.3.6" -> false, "2.13.16" -> false)
    cases.foreach { (version, expected) =>
      assertEquals(
        version,
        expected,
        ReplAction.supportsInterruptInstrumentationOption(ScalaVersion.fromString(version))
      )
    }
    assertFalse(ReplAction.supportsInterruptInstrumentationOption(None))

  @Test
  def testConfigurationSilencesTheJLineLogger(): Unit =
    val configuration = configuredForModule

    val option = ReplAction.quietJLineLoggingOption().getOrElse(fail("no logging option written"))
    assertTrue(configuration.javaOptions, configuration.javaOptions.contains(option.toString))

    val configFile = Path.of(option.toString.stripPrefix("-Djava.util.logging.config.file="))
    assertTrue(s"$configFile was not written", Files.exists(configFile))
    assertTrue(
      Files.readString(configFile),
      Files.readString(configFile).contains("org.jline.level=OFF")
    )

  @Test
  def testAStaleLoggingConfigurationIsReplacedWhole(): Unit =
    // The file is written beside and moved into place, never read or truncated, so a REPL JVM
    // starting at the same moment sees the old content or the new, and bytes that are not valid
    // UTF-8 cannot make the option vanish.
    val directory = Files.createTempDirectory("replace-jline")
    val config    = directory.resolve("replace-jline-logging.properties")
    try
      Files.write(config, Array[Byte](0xe4.toByte))

      assertTrue(ReplAction.quietJLineLoggingOption(directory.toString).isDefined)

      assertTrue(Files.readString(config), Files.readString(config).contains("org.jline.level=OFF"))
      assertEquals("no temporary file may be left behind", 1, directory.toFile.list().length)
    finally
      Files.deleteIfExists(config)
      Files.deleteIfExists(directory)

  @Test
  def testTheLoggingOptionSurvivesADirectoryWithASpace(): Unit =
    // The Scala plugin re-tokenizes javaOptions on whitespace outside double quotes, so an
    // unquoted temp path under a Windows user name with a space would split the option in two.
    val directory = Files.createTempDirectory("replace test")
    val written   = directory.resolve("replace-jline-logging.properties")
    try
      val option = ReplAction
        .quietJLineLoggingOption(directory.toString)
        .getOrElse(fail("no logging option written"))
        .toString
      assertEquals(
        option,
        List(s"-Djava.util.logging.config.file=$written"),
        ParametersListUtil.parse(option).asScala.toList
      )
    finally
      Files.deleteIfExists(written)
      Files.deleteIfExists(directory)
