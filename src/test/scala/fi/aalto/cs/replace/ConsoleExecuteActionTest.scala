package fi.aalto.cs.replace

import com.intellij.execution.console.ConsoleHistoryController
import com.intellij.execution.process.ProcessHandler
import com.intellij.execution.ui.ConsoleViewContentType
import com.intellij.openapi.actionSystem.CommonDataKeys
import com.intellij.openapi.actionSystem.impl.SimpleDataContext
import com.intellij.openapi.application.ApplicationManager
import com.intellij.openapi.util.Disposer
import com.intellij.testFramework.{TestActionEvent, fixtures}
import org.jetbrains.plugins.scala.console.ScalaConsoleInfo
import org.jetbrains.plugins.scala.console.replace.ConsoleExecuteAction
import org.junit.Assert.{assertEquals, assertFalse, assertTrue}

import java.io.{ByteArrayOutputStream, IOException, OutputStream}
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}
import java.util.Collections
import scala.jdk.CollectionConverters.*

class ConsoleExecuteActionTest extends fixtures.BasePlatformTestCase:

  def testPreservesInputUntilStartupCommandsHaveFinished(): Unit =
    withConsole(startupComplete = false) { (repl, process) =>
      val input = "println(42)"
      setConsoleText(repl, input)
      val context = SimpleDataContext.getSimpleContext(CommonDataKeys.EDITOR, repl.getConsoleEditor)
      val action  = new ConsoleExecuteAction()

      action.actionPerformed(TestActionEvent.createTestEvent(context))

      assertEquals("", process.stdinText)
      assertEquals(input, repl.getEditorDocument.getText)
    }

  def testDeletesEachMultilineSubmissionAtTheFollowingPrompt(): Unit =
    withStartedConsole { (repl, process) =>
      val firstSource = """|class First:
                           |  def first = 1
                           |
                           |  def second = 2
                           |end First
                           |""".stripMargin
      val firstFile = executeMultiline(repl, process, firstSource)
      assertEquals(firstSource, Files.readString(firstFile, UTF_8))
      assertFalse(repl.isReadyForUserInput)

      repl.print("scala> ", ConsoleViewContentType.NORMAL_OUTPUT)
      assertFalse(Files.exists(firstFile))
      assertTrue(repl.isReadyForUserInput)

      val secondSource = """|class Second:
                            |  def value = First().first + First().second
                            |
                            |  def label = "ready"
                            |end Second
                            |""".stripMargin
      val secondFile = executeMultiline(repl, process, secondSource)
      assertEquals(secondSource, Files.readString(secondFile, UTF_8))

      repl.print("scala> ", ConsoleViewContentType.NORMAL_OUTPUT)
      assertFalse(Files.exists(secondFile))
    }

  def testDoesNotTreatACompleteOutputLineAsAReplPrompt(): Unit =
    withStartedConsole { (repl, process) =>
      val sourceFile = executeMultiline(repl, process, "println(\"scala>\")\nThread.sleep(100)")

      repl.print("scala> \n", ConsoleViewContentType.NORMAL_OUTPUT)
      assertTrue(Files.exists(sourceFile))
      assertFalse(repl.isReadyForUserInput)

      repl.print("scala>", ConsoleViewContentType.NORMAL_OUTPUT)
      assertTrue(Files.exists(sourceFile))
      assertFalse(repl.isReadyForUserInput)

      repl.print(" and more output\n", ConsoleViewContentType.NORMAL_OUTPUT)
      assertTrue(Files.exists(sourceFile))
      assertFalse(repl.isReadyForUserInput)

      repl.print("scala> ", ConsoleViewContentType.NORMAL_OUTPUT)
      assertFalse(Files.exists(sourceFile))
      assertTrue(repl.isReadyForUserInput)
    }

  def testDeletesSubmissionWhenPromptArrivesSplitAcrossChunks(): Unit =
    withStartedConsole { (repl, process) =>
      val sourceFile = executeMultiline(repl, process, "val first = 1\nval second = 2")

      repl.print("val first: Int = 1\nscala>", ConsoleViewContentType.NORMAL_OUTPUT)
      assertTrue(Files.exists(sourceFile))
      assertFalse(repl.isReadyForUserInput)

      repl.print(" ", ConsoleViewContentType.NORMAL_OUTPUT)
      assertFalse(Files.exists(sourceFile))
      assertTrue(repl.isReadyForUserInput)
    }

  def testDeletesSubmissionWhenPromptArrivesAtTheEndOfAMixedChunk(): Unit =
    withStartedConsole { (repl, process) =>
      val sourceFile = executeMultiline(repl, process, "val first = 1\nval second = 2")

      repl.print("val first: Int = 1\nscala> ", ConsoleViewContentType.NORMAL_OUTPUT)
      assertFalse(Files.exists(sourceFile))
      assertTrue(repl.isReadyForUserInput)
    }

  def testPreservesInputAndReadinessWhenSubmissionWritingFails(): Unit =
    withStartedConsole { (repl, process) =>
      val source = "val first = 1\nval second = 2"
      process.clearInput()
      setConsoleText(repl, source)
      process.failWrites = true
      val context = SimpleDataContext.getSimpleContext(CommonDataKeys.EDITOR, repl.getConsoleEditor)

      new ConsoleExecuteAction().actionPerformed(TestActionEvent.createTestEvent(context))

      assertEquals(source, repl.getEditorDocument.getText)
      assertFalse(
        "no :load command may be sent when writing fails",
        process.stdinChunks.exists(_.contains(":load"))
      )
      assertTrue(repl.isReadyForUserInput)
    }

  def testDeletesPendingSubmissionWhenTheProcessTerminates(): Unit =
    withStartedConsole { (repl, process) =>
      val sourceFile = executeMultiline(repl, process, "val lines = \"\"\"first\n\nsecond\"\"\"")
      assertTrue(Files.exists(sourceFile))

      process.terminate()
      assertFalse(Files.exists(sourceFile))
    }

  def testRetriesFailedPromptCleanupWhenTheProcessTerminates(): Unit =
    withStartedConsole { (repl, process) =>
      val sourceFile = executeMultiline(repl, process, "val answer = 42\n")
      Files.delete(sourceFile)
      Files.createDirectory(sourceFile)
      val blockingFile = Files.writeString(sourceFile.resolve("still-in-use"), "test", UTF_8)

      repl.print("scala> ", ConsoleViewContentType.NORMAL_OUTPUT)
      assertTrue(Files.exists(sourceFile))

      Files.delete(blockingFile)
      process.terminate()
      assertFalse(Files.exists(sourceFile))
    }

  def testLeavesEditorAndHistoryUntouchedWhenSubmissionPreparationFails(): Unit =
    withConsole(startupComplete = true, failPreparation = true) { (repl, process) =>
      val source = "val first = 1\nval second = 2"
      setConsoleText(repl, source)
      val context = SimpleDataContext.getSimpleContext(CommonDataKeys.EDITOR, repl.getConsoleEditor)

      new ConsoleExecuteAction().actionPerformed(TestActionEvent.createTestEvent(context))

      assertEquals(source, repl.getEditorDocument.getText)
      assertFalse(
        "no :load command may be sent when preparation fails",
        process.stdinChunks.exists(_.contains(":load"))
      )
      assertTrue(repl.isReadyForUserInput)
      assertFalse(
        "history must not contain a submission that was never sent",
        repl.getHistoryViewer.getDocument.getText.contains("val first")
      )
    }

  def testDoesNothingForAnEditorThatIsNotAConsole(): Unit =
    myFixture.configureByText("NotAConsole.scala", "object NotAConsole")
    val context = SimpleDataContext.getSimpleContext(CommonDataKeys.EDITOR, myFixture.getEditor)

    // Must not throw even though no Scala console is attached to the editor.
    new ConsoleExecuteAction().actionPerformed(TestActionEvent.createTestEvent(context))

  private def executeMultiline(repl: Repl, process: RecordingProcessHandler, source: String): Path =
    process.clearInput()
    setConsoleText(repl, source)
    val context = SimpleDataContext.getSimpleContext(CommonDataKeys.EDITOR, repl.getConsoleEditor)

    new ConsoleExecuteAction().actionPerformed(TestActionEvent.createTestEvent(context))

    val command = process.stdinChunks
      .find(_.startsWith(":load "))
      .getOrElse(throw new AssertionError(s"no :load command sent, got: ${process.stdinChunks}"))
    Path.of(command.stripSuffix("\n").stripPrefix(":load "))

  private def setConsoleText(repl: Repl, source: String): Unit =
    ApplicationManager.getApplication.runWriteAction(
      new Runnable:
        override def run(): Unit = repl.getEditorDocument.setText(source)
    )

  @annotation.nowarn("cat=deprecation")
  private def withStartedConsole(test: (Repl, RecordingProcessHandler) => Unit): Unit =
    withConsole(startupComplete = true)(test)

  @annotation.nowarn("cat=deprecation")
  private def withConsole(startupComplete: Boolean, failPreparation: Boolean = false)(
      test: (Repl, RecordingProcessHandler) => Unit
  ): Unit =
    val repl = new Repl(getModule):
      override private[replace] def initialCommands: List[String] = List.empty
      override private[replace] def scala3Module: Boolean         = true
      override def prepareMultilineSubmission(source: String): Scala3MultilineSubmission =
        if failPreparation then throw new IOException("simulated temporary file failure")
        else super.prepareMultilineSubmission(source)
    val process    = new RecordingProcessHandler
    val controller = new ConsoleHistoryController("REPLace test", "multiline", repl)
    repl.getComponent
    repl.attachToProcess(process)
    process.startNotify()
    ScalaConsoleInfo.addConsole(repl, controller, process)
    if startupComplete then
      repl.print(
        "Type in expressions for evaluation. Or try :help.\n",
        ConsoleViewContentType.NORMAL_OUTPUT
      )
      repl.print("scala> ", ConsoleViewContentType.NORMAL_OUTPUT)

    try test(repl, process)
    finally
      ScalaConsoleInfo.disposeConsole(repl)
      controller.dispose()
      Disposer.dispose(repl)
