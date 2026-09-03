package fi.aalto.cs.replace

import com.intellij.execution.ui.ConsoleViewContentType
import com.intellij.openapi.util.Disposer
import fi.aalto.cs.replace.utils.ModuleUtils
import org.junit.Assert.{assertEquals, assertFalse, assertTrue}
import org.junit.jupiter.api.Test

import java.nio.file.{Files, Path}
import java.util.concurrent.{CountDownLatch, TimeUnit}

class ReplStartupCommandsTest extends ReplPlatformTestBase:

  /** A console whose startup commands come from the module's real `.repl-commands` file, the way
    * production builds them.
    */
  private def replReadingItsCommandsFile(scala3: Boolean = true): Repl =
    new Repl(getModule):
      override private[replace] def scala3Module: Boolean = scala3

  private def commandsFile: Path =
    Path.of(ModuleUtils.getModuleDirectory(getModule), ".repl-commands")

  /** The one command the console feeds, which loads the whole file as a single compilation unit. */
  private def loadCommand: String = s":load $commandsFile\n"

  /** Runs `test` on a console of a module whose `.repl-commands` file holds `content`. */
  private def withCommandsFile(content: String, scala3: Boolean = true)(
      test: (Repl, RecordingProcessHandler) => Unit
  ): Unit =
    Files.writeString(commandsFile, content)
    try withRepl(replReadingItsCommandsFile(scala3))(test)
    finally Files.deleteIfExists(commandsFile)

  @Test
  def testLoadsTheModulesCommandsFileAtTheFirstPrompt(): Unit =
    withCommandsFile("import o1.*\nval greeting = \"hi\"\n") { (repl, process) =>
      repl.print(Repl.welcomeLine, ConsoleViewContentType.NORMAL_OUTPUT)
      assertEquals("nothing may be sent before a prompt", "", process.stdinText)
      assertFalse(repl.isReadyForUserInput)

      repl.print("scala> ", ConsoleViewContentType.NORMAL_OUTPUT)
      assertEquals(loadCommand, process.stdinText)
      assertFalse(repl.isReadyForUserInput)

      repl.print("scala> ", ConsoleViewContentType.NORMAL_OUTPUT)
      assertEquals("the file is loaded once", loadCommand, process.stdinText)
      assertTrue(repl.isReadyForUserInput)
    }

  @Test
  def testAMultiLineCommandsFileIsLoadedAsOneUnit(): Unit =
    // One line per prompt would wedge the console here: dotty answers an incomplete line with a
    // continuation prompt that a dumb terminal never prints, so no prompt would ever arrive to
    // send the rest of the file.
    withCommandsFile("def answer =\n  42\nimport o1.*\n") { (repl, process) =>
      repl.print(Repl.welcomeLine, ConsoleViewContentType.NORMAL_OUTPUT)
      repl.print("scala> ", ConsoleViewContentType.NORMAL_OUTPUT)
      assertEquals(loadCommand, process.stdinText)

      repl.print("scala> ", ConsoleViewContentType.NORMAL_OUTPUT)
      assertTrue(repl.isReadyForUserInput)
    }

  @Test
  def testAPromptSplitAcrossChunksStillLoadsTheCommandsFile(): Unit =
    // Classification is pinned in ReplOutputTrackerTest; this exercises the pump on the chunk that
    // completes the prompt.
    withCommandsFile("import o1.*\n") { (repl, process) =>
      repl.print(Repl.welcomeLine, ConsoleViewContentType.NORMAL_OUTPUT)
      repl.print("scala>", ConsoleViewContentType.NORMAL_OUTPUT)
      assertEquals("", process.stdinText)
      repl.print(" ", ConsoleViewContentType.NORMAL_OUTPUT)
      assertEquals(loadCommand, process.stdinText)
    }

  @Test
  def testACommandsFileThatIsNotUtf8IsStillLoaded(): Unit =
    // The REPL decodes the file itself, so an undecodable byte may cost the welcome text's import
    // summary but never the commands, and never the launch.
    Files.write(commandsFile, Array[Byte](0xe4.toByte, '\n'.toByte))
    try
      withRepl(replReadingItsCommandsFile()) { (repl, process) =>
        repl.print(Repl.welcomeLine, ConsoleViewContentType.NORMAL_OUTPUT)
        repl.print("scala> ", ConsoleViewContentType.NORMAL_OUTPUT)
        assertEquals(loadCommand, process.stdinText)

        repl.print("scala> ", ConsoleViewContentType.NORMAL_OUTPUT)
        assertTrue(repl.isReadyForUserInput)
      }
    finally Files.deleteIfExists(commandsFile)

  @Test
  def testAModuleWithoutACommandsFileFeedsNothing(): Unit =
    assertFalse(s"$commandsFile must not exist for this test", Files.exists(commandsFile))
    withRepl(replReadingItsCommandsFile()) { (repl, process) =>
      repl.print(Repl.welcomeLine, ConsoleViewContentType.NORMAL_OUTPUT)
      repl.print("scala> ", ConsoleViewContentType.NORMAL_OUTPUT)

      assertEquals("", process.stdinText)
      assertTrue(repl.isReadyForUserInput)
    }

  @Test
  def testAWelcomeLineSplitAcrossChunksStillArmsTheStartupCommands(): Unit =
    withCommandsFile("import o1.*\n") { (repl, process) =>
      repl.print("Type in expressions for eval", ConsoleViewContentType.NORMAL_OUTPUT)
      repl.print("uation. Or try :help.\n", ConsoleViewContentType.NORMAL_OUTPUT)
      repl.print("scala> ", ConsoleViewContentType.NORMAL_OUTPUT)
      assertEquals(loadCommand, process.stdinText)
    }

  @Test
  def testAReplWhoseBannerReadsDifferentlyStillLoadsItsCommandsFile(): Unit =
    // The welcome line is the usual trigger, but a REPL that words its banner differently must not
    // lose its startup commands. The first prompt arms them instead, and releases the held banner.
    withCommandsFile("import o1.*\n") { (repl, process) =>
      repl.print("Type in expressions to have them", ConsoleViewContentType.NORMAL_OUTPUT)
      repl.print(" evaluated.\nscala> ", ConsoleViewContentType.NORMAL_OUTPUT)
      repl.flushDeferredText()

      assertEquals(loadCommand, process.stdinText)
      assertFalse(repl.isReadyForUserInput)
      val history = repl.getHistoryViewer.getDocument.getText
      assertTrue(history, history.contains("Type in expressions to have them evaluated.\n"))
    }

  @Test
  def testAFailingStartupCommandDoesNotStrandTheConsole(): Unit =
    // The command is already popped when the write fails, and readiness must be restored, or no
    // prompt would ever arrive to un-stick the console.
    withCommandsFile("import o1.*\n") { (repl, process) =>
      repl.print(Repl.welcomeLine, ConsoleViewContentType.NORMAL_OUTPUT)
      process.failWrites = true
      repl.print("scala> ", ConsoleViewContentType.NORMAL_OUTPUT)
      assertTrue(repl.isReadyForUserInput)
    }

  @Test
  def testScala2DoesNotAlsoPumpCommandsHandledByItsInitFile(): Unit =
    withCommandsFile("def twice(n: Int) =\n  n * 2\n", scala3 = false) { (repl, process) =>
      repl.print(Repl.welcomeLine, ConsoleViewContentType.NORMAL_OUTPUT)
      repl.print("scala> ", ConsoleViewContentType.NORMAL_OUTPUT)

      assertEquals("Scala 2 startup commands are loaded by -i", "", process.stdinText)
      assertTrue(repl.isReadyForUserInput)
    }

  @Test
  def testAScala2ConsoleIsReadyWithoutAPrompt(): Unit =
    // Only Scala 3 paces its input against the prompt. A Scala 2 prompt can be reconfigured away,
    // so gating Scala 2 on one would leave the console un-enterable for good.
    withRepl(replReadingItsCommandsFile(scala3 = false)) { (repl, process) =>
      assertTrue(repl.isReadyForUserInput)
      process.startNotify()
      process.terminate()
      assertFalse("a dead process accepts no input", repl.isReadyForUserInput)
    }

  @Test
  def testAttachingAnAlreadyTerminatedProcessKeepsTheConsoleUnready(): Unit =
    val process = new RecordingProcessHandler
    process.startNotify()
    process.terminate()
    val repl = new Repl(getModule):
      override private[replace] def initialCommandsFile: Option[Path] = None
      override private[replace] def scala3Module: Boolean             = true

    repl.attachToProcess(process)
    try
      repl.print(Repl.welcomeLine, ConsoleViewContentType.NORMAL_OUTPUT)
      repl.print("scala> ", ConsoleViewContentType.SYSTEM_OUTPUT)
      assertFalse(repl.isReadyForUserInput)
    finally Disposer.dispose(repl)

  @Test
  def testLoadingInitialCommandsDuringAttachDoesNotHoldTheOutputStateLock(): Unit =
    whileInitialCommandLoadingIsBlocked("test-repl-attach") { (repl, process) =>
      val done = new CountDownLatch(1)
      // The two things that need the console's state lock while an attach reads the commands
      // file: the REPL process dying, and an Enter asking how to transport its input.
      val contender = new Thread(
        () =>
          try
            process.terminate()
            val _ = repl.mustUseRawTransport
          finally done.countDown(),
        "test-repl-termination"
      )
      contender.start()
      // Shorter than the deadline the parked attach gives up on, or a lock held for the whole
      // park would look like it was released in time.
      assertTrue(
        "console state must not wait for initial-command read access",
        done.await(2, TimeUnit.SECONDS)
      )
      contender.join(5000)
      assertFalse("the contending work must finish", contender.isAlive)
    }

  @Test
  def testAttachDoesNotResumeAfterDisposalDuringInitialCommandLoading(): Unit =
    val repl = whileInitialCommandLoadingIsBlocked("test-repl-late-attach") { (console, _) =>
      Disposer.dispose(console)
    }
    assertTrue("the console must remain disposed", repl.isReplDisposed)
    assertTrue("a disposed console must not retain the process", repl.attachedProcessInput.isEmpty)

  /** Runs `body` on a console whose attach is parked inside initial-command loading. */
  private def whileInitialCommandLoadingIsBlocked(attachThreadName: String)(
      body: (Repl, RecordingProcessHandler) => Unit
  ): Repl =
    val loadingStarted = new CountDownLatch(1)
    val allowLoading   = new CountDownLatch(1)
    val process        = new RecordingProcessHandler
    // Without this the handler stays in its initial state and queues termination instead of
    // notifying the listener, so no thread would ever contend for the console's state lock.
    process.startNotify()
    val repl = new Repl(getModule):
      override private[replace] def initialCommandsFile: Option[Path] =
        loadingStarted.countDown()
        assertTrue(
          "the test must release initial-command loading",
          allowLoading.await(5, TimeUnit.SECONDS)
        )
        None
      override private[replace] def scala3Module: Boolean = true

    val attachThread = new Thread(() => repl.attachToProcess(process), attachThreadName)
    try
      attachThread.start()
      assertTrue("initial-command loading must start", loadingStarted.await(5, TimeUnit.SECONDS))
      body(repl, process)
    finally
      allowLoading.countDown()
      attachThread.join(5000)
      if !repl.isReplDisposed then Disposer.dispose(repl)
    assertFalse("process attachment must finish", attachThread.isAlive)
    repl
