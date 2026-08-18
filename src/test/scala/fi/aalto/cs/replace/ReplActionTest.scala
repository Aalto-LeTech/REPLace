package fi.aalto.cs.replace

import com.intellij.testFramework.fixtures.BasePlatformTestCase
import fi.aalto.cs.replace.actions.ReplAction
import org.jetbrains.plugins.scala.console.configuration.ScalaConsoleRunConfiguration
import org.junit.Assert.{assertEquals, assertFalse, assertTrue, fail}

import java.nio.file.{Files, Path}

/** See [[ReplAction]] for why the Scala plugin's experimental console-configuration API is used
  * (and its inspection suppressed) rather than a stable equivalent.
  */
//noinspection UnstableApiUsage
class ReplActionTest extends BasePlatformTestCase:

  private def newConfiguration: ScalaConsoleRunConfiguration =
    new ReplAction().configurationFactory
      .createTemplateConfiguration(getProject)
      .asInstanceOf[ScalaConsoleRunConfiguration]

  def testConfigurationFactoryIdIsStableAcrossLocales(): Unit =
    // The factory ID is persisted in run configurations and must never be localized.
    assertEquals("Enhanced Scala REPL", new ReplAction().configurationFactory.getId)

  def testConfigurationEnablesNativeAccessForTheReplJvm(): Unit =
    val configuration = newConfiguration
    ReplAction.configureForModule(configuration, getModule)

    assertTrue(
      configuration.javaOptions,
      configuration.javaOptions.contains("--enable-native-access=ALL-UNNAMED")
    )

  def testConfigurationUsesTheJavaClasspath(): Unit =
    val configuration = newConfiguration
    ReplAction.configureForModule(configuration, getModule)

    assertTrue(configuration.myConsoleArgs, configuration.myConsoleArgs.contains("-usejavacp"))

  def testUnsafeMemoryAccessIsAllowedOnJdk23AndNewer(): Unit =
    // Scala's LazyVals use sun.misc.Unsafe, which JDK 24+ warns about on REPL startup; the
    // silencing option exists since JDK 23.
    assertTrue(ReplAction.supportsUnsafeMemoryAccessOption(Some("openjdk version \"25.0.3\"")))
    assertTrue(ReplAction.supportsUnsafeMemoryAccessOption(Some("java version \"23\"")))

  def testUnsafeMemoryAccessOptionIsOmittedOnOlderJdks(): Unit =
    // JDKs older than 23 do not recognize the option and would refuse to start.
    assertFalse(ReplAction.supportsUnsafeMemoryAccessOption(Some("openjdk version \"17.0.9\"")))
    assertFalse(ReplAction.supportsUnsafeMemoryAccessOption(Some("version 11.0.2")))
    assertFalse(ReplAction.supportsUnsafeMemoryAccessOption(None))
    assertFalse(ReplAction.supportsUnsafeMemoryAccessOption(Some("not a version")))

  def testConfigurationOmitsUnsafeMemoryAccessWithoutAJdk(): Unit =
    val configuration = newConfiguration
    ReplAction.configureForModule(configuration, getModule)

    assertFalse(
      configuration.javaOptions,
      configuration.javaOptions.contains("--sun-misc-unsafe-memory-access")
    )

  def testConfigurationTellsJLineTheTerminalIsDumb(): Unit =
    // The console gives the REPL pipes, not a pty; without this JLine warns about the fallback.
    val configuration = newConfiguration
    ReplAction.configureForModule(configuration, getModule)

    assertTrue(
      configuration.javaOptions,
      configuration.javaOptions.contains("-Dorg.jline.terminal.dumb=true")
    )

  def testConfigurationSilencesTheJLineLogger(): Unit =
    val configuration = newConfiguration
    ReplAction.configureForModule(configuration, getModule)

    val option = ReplAction.quietJLineLoggingOption.getOrElse(fail("no logging option written"))
    assertTrue(configuration.javaOptions, configuration.javaOptions.contains(option.toString))

    val configFile = Path.of(option.toString.stripPrefix("-Djava.util.logging.config.file="))
    assertTrue(s"$configFile was not written", Files.exists(configFile))
    assertTrue(
      Files.readString(configFile),
      Files.readString(configFile).contains("org.jline.level=OFF")
    )
