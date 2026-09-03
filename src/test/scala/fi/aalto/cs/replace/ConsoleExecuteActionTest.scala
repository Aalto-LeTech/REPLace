package fi.aalto.cs.replace

import com.intellij.execution.ui.ConsoleViewContentType
import com.intellij.notification.{Notification, NotificationAction, NotificationsManager}
import com.intellij.openapi.actionSystem.CommonDataKeys
import com.intellij.openapi.actionSystem.impl.SimpleDataContext
import com.intellij.openapi.application.WriteAction
import com.intellij.openapi.editor.EditorFactory
import com.intellij.openapi.fileEditor.FileDocumentManager
import com.intellij.openapi.roots.ModuleRootManager
import com.intellij.openapi.util.Disposer
import com.intellij.openapi.vfs.VirtualFile
import com.intellij.testFramework.TestActionEvent
import fi.aalto.cs.replace.actions.SendSelectionToConsoleAction
import fi.aalto.cs.replace.utils.MyBundle
import org.jetbrains.plugins.scala.LatestScalaVersions
import org.jetbrains.plugins.scala.console.ScalaConsoleInfo
import org.jetbrains.plugins.scala.console.replace.ConsoleExecuteAction
import org.jetbrains.plugins.scala.extensions.inWriteAction
import org.jetbrains.plugins.scala.project.ScalaFeatures
import org.junit.Assert.{assertEquals, assertFalse, assertTrue}
import org.junit.jupiter.api.Test

import java.io.IOException
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}
import java.util.concurrent.atomic.AtomicBoolean
import scala.collection.mutable

class ConsoleExecuteActionTest extends ReplPlatformTestBase:

  /** The paste this feature exists for, and the echoes the REPL prints back for it. */
  private val reassignmentPaste  = "var a = 1\na = a + 1\na = a + 1"
  private val reassignmentEchoes = Seq("var a: Int = 1", "a: Int = 2", "a: Int = 3")
  private val promptOutputType   = ConsoleViewContentType.NORMAL_OUTPUT
  private val styledOutputType   = ConsoleViewContentType.SYSTEM_OUTPUT

  /** Every submission file the console under test created, for asserting cleanup. */
  private val createdSubmissionFiles = mutable.Buffer.empty[Path]

  /** The title the busy-input hint carries, for finding those hints among the project's. */
  private val busyTitle = MyBundle.message("ui.repl.console.input.busy.title")

  @Test
  def testPreservesInputAndOffersFeedbackWhileTheConsoleIsNotReady(): Unit =
    withConsole(startupComplete = false) { (repl, process) =>
      val input = "println(42)"
      executePaste(repl, process, input)

      assertEquals("", process.stdinText)
      assertEquals(input, repl.getEditorDocument.getText)
      assertTrue(unavailableHints().nonEmpty)
    }

  @Test
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

      prompt(repl)
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

      prompt(repl)
      assertFalse(Files.exists(secondFile))
    }

  @Test
  def testPacesSplitStatementsOneLoadPerPrompt(): Unit =
    withStartedConsole { (repl, process) =>
      executePaste(repl, process, reassignmentPaste)

      val firstFile = loadedFile(process, 0)
      assertEquals("var a = 1", Files.readString(firstFile, UTF_8).trim)
      assertFalse(repl.isReadyForUserInput)

      prompt(repl)
      val secondFile = loadedFile(process, 1)
      assertEquals("a = a + 1", Files.readString(secondFile, UTF_8).trim)
      assertFalse(Files.exists(firstFile))
      assertFalse(repl.isReadyForUserInput)

      prompt(repl)
      val thirdFile = loadedFile(process, 2)
      assertEquals("a = a + 1", Files.readString(thirdFile, UTF_8).trim)
      assertFalse(Files.exists(secondFile))
      assertFalse(repl.isReadyForUserInput)

      prompt(repl)
      assertFalse(Files.exists(thirdFile))
      assertTrue(repl.isReadyForUserInput)
    }

  @Test
  def testDifferentlyStyledHiddenPromptStillConsumesItsSpacingNewline(): Unit =
    withConsole(startupComplete = true, startupPromptType = styledOutputType) { (repl, process) =>
      executePaste(repl, process, reassignmentPaste)

      repl.print("var a: Int = 1\n\n", ConsoleViewContentType.NORMAL_OUTPUT)
      repl.print("scala> ", styledOutputType)
      repl.flushDeferredText()

      val history = repl.getHistoryViewer.getDocument.getText
      assertTrue(
        s"a content-type change at the prompt must not add a blank line, got:\n$history",
        outputRegion(history).endsWith("var a: Int = 1\n")
      )
      assertEquals(2, loadedFiles(process).size)
    }

  @Test
  def testFlushesHeldStdoutBeforeDifferentlyStyledOutput(): Unit =
    withStartedConsole { (repl, process) =>
      executePaste(repl, process, reassignmentPaste)

      repl.print("var a: Int = 1\n\n", ConsoleViewContentType.NORMAL_OUTPUT)
      repl.print("warning", ConsoleViewContentType.ERROR_OUTPUT)
      repl.flushDeferredText()

      val history = repl.getHistoryViewer.getDocument.getText
      assertTrue(
        s"withheld stdout must stay before the following styled output, got:\n$history",
        history.endsWith("var a: Int = 1\n\nwarning")
      )
    }

  @Test
  def testNonStdoutOutputDoesNotLoseAPromptSplitAroundIt(): Unit =
    // stderr is written by a different stream and can land between the two halves of a prompt.
    // Losing that prompt would strand the paste, since only a prompt advances it.
    withStartedConsole { (repl, process) =>
      executePaste(repl, process, reassignmentPaste)
      val firstFile = loadedFile(process, 0)

      repl.print("\nsca", ConsoleViewContentType.NORMAL_OUTPUT)
      repl.print("warning", ConsoleViewContentType.ERROR_OUTPUT)
      repl.print("la> ", ConsoleViewContentType.NORMAL_OUTPUT)

      assertEquals("the completed prompt must advance the paste", 2, loadedFiles(process).size)
      assertFalse("the acknowledged submission must be deleted", Files.exists(firstFile))
      assertFalse("the split paste still has another group", repl.isReadyForUserInput)
    }

  @Test
  def testDisambiguatedColoredPromptPrefixKeepsItsStyle(): Unit =
    withStartedConsole { (repl, process) =>
      executePaste(repl, process, reassignmentPaste)

      // `scala` is held as a possible prompt prefix. The NORMAL_OUTPUT suffix proves that it was
      // ordinary colored output, so the prefix must be flushed with its original attributes.
      repl.print("\nscala", styledOutputType)
      repl.print("fish", ConsoleViewContentType.NORMAL_OUTPUT)
      repl.flushDeferredText()

      val text = repl.getHistoryViewer.getDocument.getText
      assertTrue(
        "a differently styled non-prompt suffix must flush the held prefix with its old style, " +
          s"but the history was:\n$text",
        text.endsWith("\nscalafish")
      )
    }

  @Test
  def testPadsEachSplitFileToItsOriginalPasteLine(): Unit =
    withStartedConsole { (repl, process) =>
      val source = "\n\nvar a = 1\n\na =\n  a + 1\nprintln(a)"
      executePaste(repl, process, source)

      val firstFile = loadedFile(process, 0)
      assertEquals("\n\nvar a = 1", Files.readString(firstFile, UTF_8))

      prompt(repl)
      val secondFile = loadedFile(process, 1)
      assertEquals("\n" * 4 + "a =\n  a + 1", Files.readString(secondFile, UTF_8))

      prompt(repl)
      val thirdFile = loadedFile(process, 2)
      assertEquals("\n" * 6 + "println(a)", Files.readString(thirdFile, UTF_8))
    }

  @Test
  def testDeletesTheWholeQueueWhenTheProcessTerminates(): Unit =
    withStartedConsole { (repl, process) =>
      executePaste(repl, process, "var a = 1\na = a + 1")
      val firstFile = loadedFiles(process).head
      assertTrue(Files.exists(firstFile))

      process.terminate()
      assertNoSubmissionFilesRemain()
      // The queued second statement must not be written to a dead process.
      assertEquals(1, loadedFiles(process).size)
    }

  @Test
  def testChunkingVariantsAllRenderConsecutiveEchoes(): Unit =
    // The console-level anchor for chunk-boundary independence. It pins the wiring the plain
    // ReplSpacingFilterTest harness fakes, namely the history document, queue pump and readiness.
    val variants: List[List[String]] = List(
      List("var a: Int = 1\n\n"),        // echo and spacing in one chunk
      List("var a: Int = 1", "\n\n"),    // terminator and spacing together
      List("var a: Int = 1", "\n", "\n") // fully exploded
    )
    for variant <- variants do
      withStartedConsole { (repl, process) =>
        executePaste(repl, process, reassignmentPaste)

        for echo <- reassignmentEchoes do
          for chunk <- variant do
            repl.print(chunk.replace("var a: Int = 1", echo), ConsoleViewContentType.NORMAL_OUTPUT)
          prompt(repl)
        repl.flushDeferredText()

        val history = repl.getHistoryViewer.getDocument.getText
        assertEquals(
          s"variant $variant must render identically, got:\n$history",
          "var a: Int = 1\na: Int = 2\na: Int = 3\nscala> ",
          outputRegion(history)
        )
        assertTrue(repl.isReadyForUserInput)
      }

  @Test
  def testPromptLikeOutputIsReclaimedByTheFollowingChunk(): Unit =
    // Output that merely looks like the start of a prompt is ambiguous and is held back, but only
    // until the next chunk, which must print it in full and in order.
    withStartedConsole { (repl, process) =>
      executeMultiline(repl, process, "val first =\n  1")

      repl.print("out\n", ConsoleViewContentType.NORMAL_OUTPUT)
      repl.print("scala", ConsoleViewContentType.NORMAL_OUTPUT)
      repl.print("fish\n", ConsoleViewContentType.NORMAL_OUTPUT)
      repl.flushDeferredText()

      val history = repl.getHistoryViewer.getDocument.getText
      assertTrue(
        s"the held prompt prefix must be reclaimed by the next chunk, got:\n$history",
        history.endsWith("out\nscalafish\n")
      )
    }

  @Test
  def testFlushesHeldBackTextWhenTheProcessDies(): Unit =
    // Text held back mid-paste is reclaimed by the next chunk; when the process dies there is no
    // next chunk, so the held text must be printed rather than silently dropped.
    withStartedConsole { (repl, process) =>
      executePaste(repl, process, reassignmentPaste)

      repl.print("var a: Int = 1\n\nscal", ConsoleViewContentType.NORMAL_OUTPUT)
      process.terminate()
      repl.flushDeferredText()

      val history = repl.getHistoryViewer.getDocument.getText
      assertTrue(
        s"held-back output must be flushed on termination, got:\n$history",
        history.endsWith("var a: Int = 1\n\nscal")
      )
    }

  @Test
  def testSurfacesAMidQueueWriteFailureInTheConsole(): Unit =
    withStartedConsole { (repl, process) =>
      executePaste(repl, process, reassignmentPaste)

      process.failWrites = true
      repl.print("var a: Int = 1\n\n", ConsoleViewContentType.NORMAL_OUTPUT)
      prompt(repl)
      repl.flushDeferredText()

      // The remaining two groups can never be sent: their files must be gone, the user must be
      // told in the console, and the console must end at a usable prompt.
      assertNoSubmissionFilesRemain()
      assertTrue(repl.isReadyForUserInput)
      val history = repl.getHistoryViewer.getDocument.getText
      val message = MyBundle.message("ui.repl.console.input.dropped", 2)
      assertTrue(
        s"the user must see that parts were dropped, got:\n$history",
        history.contains(message)
      )
      assertTrue(
        s"the notice must follow the echo of the part that ran, got:\n$history",
        history.indexOf("var a: Int = 1") < history.indexOf(message)
      )
      assertTrue(
        s"the console must end at a prompt, not at the notice, got:\n$history",
        history.endsWith("scala> ")
      )
    }
    // The same failure with only the last group left must report it in the singular.
    withStartedConsole { (repl, process) =>
      executePaste(repl, process, reassignmentPaste)
      prompt(repl)

      process.failWrites = true
      prompt(repl)
      repl.flushDeferredText()

      val history = repl.getHistoryViewer.getDocument.getText
      assertTrue(history, history.contains(MyBundle.message("ui.repl.console.input.dropped", 1)))
      assertNoSubmissionFilesRemain()
    }

  @Test
  def testAPromptDuringTheHeadWriteDoesNotAdvanceThePaste(): Unit =
    // Until a group's :load line has been written, no prompt can be that group's completion
    // prompt, and advancing on one would delete the group unsent while the groups after it run.
    withStartedConsole { (repl, process) =>
      val blocked = duringNextGroupWrite(process)(() => prompt(repl))
      executePaste(repl, process, reassignmentPaste)

      assertFalse("the concurrent console call must not block the writer", blocked.get)
      assertEquals("no second group may be released", 1, loadedFiles(process).size)
      assertTrue("the unsent group's file must survive", Files.exists(loadedFile(process, 0)))
      assertFalse(repl.isReadyForUserInput)
      assertEquals(
        "the sent paste must not be restored into the editor",
        "",
        repl.getEditorDocument.getText
      )

      // The real completion prompts must still drain the paste.
      prompt(repl)
      assertEquals("a = a + 1", Files.readString(loadedFile(process, 1), UTF_8).trim)
      prompt(repl)
      prompt(repl)
      assertEquals(3, loadedFiles(process).size)
      assertNoSubmissionFilesRemain()
      assertTrue(repl.isReadyForUserInput)
    }

  @Test
  def testTheHiddenPromptIsRenderedBeforeTheNextGroupIsWritten(): Unit =
    // The hidden-prompt transition completes before the next group's :load reaches the pipe, so
    // output arriving during that write lands after the whole transition, never inside it.
    withStartedConsole { (repl, process) =>
      executePaste(repl, process, reassignmentPaste)
      repl.print("var a: Int = 1\n\n", ConsoleViewContentType.NORMAL_OUTPUT)

      val blocked = duringNextGroupWrite(process)(() =>
        repl.print("concurrent warning\n", ConsoleViewContentType.ERROR_OUTPUT)
      )
      prompt(repl)
      repl.flushDeferredText()

      assertFalse("the concurrent console call must not block the writer", blocked.get)
      assertEquals(2, loadedFiles(process).size)
      val history = outputRegion(repl.getHistoryViewer.getDocument.getText)
      assertTrue(
        s"stderr must follow the complete hidden-prompt transition, got:\n$history",
        history.endsWith("var a: Int = 1\nconcurrent warning\n")
      )
      assertFalse("the split paste still has another group", repl.isReadyForUserInput)
    }

  @Test
  def testTerminationDuringTheHeadWriteAbandonsThePaste(): Unit =
    // The process can die while the head's :load is still being written. Every file must be
    // deleted, readiness cleared, and no later prompt may write to the dead process.
    withStartedConsole { (repl, process) =>
      terminateDuringNextWrite(process)
      executePaste(repl, process, reassignmentPaste)

      assertNoSubmissionFilesRemain()
      assertFalse(repl.isReadyForUserInput)
      assertEquals(
        "a first-group write that did not reach a live process must restore the editor",
        reassignmentPaste,
        repl.getEditorDocument.getText
      )

      prompt(repl)
      assertEquals("no bytes were accepted by the dead process", 0, loadedFiles(process).size)
      assertFalse(repl.isReadyForUserInput)
    }

  @Test
  def testTerminationAfterTheHeadWriteDoesNotRestoreSentInput(): Unit =
    // A process may accept and flush the :load command and then exit before writeTo returns. A
    // normal write/flush completion is the delivery boundary, or the user could send it twice.
    withStartedConsole { (repl, process) =>
      terminateAfterBytesAreAccepted(process)
      executePaste(repl, process, reassignmentPaste)

      assertEquals("", repl.getEditorDocument.getText)
      assertTrue(
        "accepted input must be retained in navigable history",
        ScalaConsoleInfo.getController(repl.getConsoleEditor).hasHistory
      )
      assertEquals(1, loadedFiles(process).size)
      assertFalse(repl.isReadyForUserInput)
      assertNoSubmissionFilesRemain("termination must delete every submission file")
    }

  @Test
  def testDropsTheRestOfThePasteWhenTheProcessInputDisappears(): Unit =
    // A live process whose input stream is gone cannot take the next group; the paste must be
    // dropped with the usual notice rather than crashing the output thread.
    withStartedConsole { (repl, process) =>
      executePaste(repl, process, reassignmentPaste)
      process.nullProcessInput = true

      prompt(repl)
      repl.flushDeferredText()

      assertNoSubmissionFilesRemain()
      val history = repl.getHistoryViewer.getDocument.getText
      assertTrue(history, history.contains(MyBundle.message("ui.repl.console.input.dropped", 2)))
      assertTrue(repl.isReadyForUserInput)
    }

  @Test
  def testDisposingMidPasteDeletesEveryRemainingFile(): Unit =
    withStartedConsole { (repl, process) =>
      executePaste(repl, process, reassignmentPaste)
      Disposer.dispose(repl)

      assertNoSubmissionFilesRemain()
      assertFalse(repl.isReadyForUserInput)
    }

  @Test
  def testDeletesSubmissionWhenPromptArrivesSplitAcrossChunks(): Unit =
    withStartedConsole { (repl, process) =>
      val sourceFile = executeMultiline(repl, process, "val first =\n  1")

      repl.print("val first: Int = 1\n", ConsoleViewContentType.NORMAL_OUTPUT)
      repl.print("scala>", promptOutputType)
      assertTrue(Files.exists(sourceFile))
      assertFalse(repl.isReadyForUserInput)

      repl.print(" ", promptOutputType)
      assertFalse(Files.exists(sourceFile))
      assertTrue(repl.isReadyForUserInput)
    }

  @Test
  def testDeletesSubmissionWhenPromptArrivesAtTheEndOfAMixedChunk(): Unit =
    withStartedConsole { (repl, process) =>
      val sourceFile = executeMultiline(repl, process, "val first =\n  1")

      repl.print("val first: Int = 1\nscala> ", promptOutputType)

      assertFalse(Files.exists(sourceFile))
      assertTrue(repl.isReadyForUserInput)
    }

  @Test
  def testEmptyStyledChunkDoesNotFlushASplitHiddenPrompt(): Unit =
    withStartedConsole { (repl, process) =>
      executePaste(repl, process, reassignmentPaste)
      val historyBeforePrompt = repl.getHistoryViewer.getDocument.getText

      repl.print("scala>", promptOutputType)
      repl.print("", styledOutputType)
      repl.print(" ", promptOutputType)
      repl.flushDeferredText()

      assertEquals(
        "an empty styled chunk must not make the intermediate prompt visible",
        historyBeforePrompt,
        repl.getHistoryViewer.getDocument.getText
      )
      assertEquals(2, loadedFiles(process).size)
    }

  @Test
  def testPreservesInputWhenASingleLineWriteFails(): Unit =
    // Single lines go to the process one by one instead of through a submission file, but a write
    // that never reached the REPL must lose no more than a multiline one does.
    withStartedConsole { (repl, process) =>
      val source = "println(42)"
      process.failWrites = true
      repl.flushDeferredText()
      val historyBefore = repl.getHistoryViewer.getDocument.getText
      setInput(repl, source)
      pressEnter(repl)
      repl.flushDeferredText()

      assertEquals(source, repl.getEditorDocument.getText)
      assertEquals(
        "visible history must not contain input that was never sent",
        historyBefore,
        repl.getHistoryViewer.getDocument.getText
      )
      assertFalse(
        "navigable history must not contain input that was never sent",
        ScalaConsoleInfo.getController(repl.getConsoleEditor).hasHistory
      )
    }

  @Test
  def testPreservesInputAndReadinessWhenSubmissionWritingFails(): Unit =
    withStartedConsole { (repl, process) =>
      val source = "val first = 1\nval second = 2"
      process.failWrites = true
      repl.flushDeferredText()
      val historyBefore = repl.getHistoryViewer.getDocument.getText
      inWriteAction {
        repl.getEditorDocument.setText(source)
        repl.getConsoleEditor.getCaretModel.moveToOffset(8)
        repl.getConsoleEditor.getSelectionModel.setSelection(4, 12)
      }
      pressEnter(repl)

      assertEquals(source, repl.getEditorDocument.getText)
      assertEquals(8, repl.getConsoleEditor.getCaretModel.getOffset)
      assertEquals(4, repl.getConsoleEditor.getSelectionModel.getSelectionStart)
      assertEquals(12, repl.getConsoleEditor.getSelectionModel.getSelectionEnd)
      assertEquals(
        "visible history must not contain a submission that was never sent",
        historyBefore,
        repl.getHistoryViewer.getDocument.getText
      )
      assertFalse(
        "navigable history must not contain a submission that was never sent",
        ScalaConsoleInfo.getController(repl.getConsoleEditor).hasHistory
      )
      assertFalse(
        "no :load command may be sent when writing fails",
        process.stdinChunks.exists(_.contains(":load"))
      )
      assertTrue(repl.isReadyForUserInput)
      assertNoSubmissionFilesRemain()
    }

  @Test
  def testRemovesTheEchoWhenTheHistoryBufferTrimsItsFront(): Unit =
    withStartedConsole { (repl, process) =>
      val source = "val first = 1\nval second = 2"
      process.failWrites = true
      repl.flushDeferredText()
      val history = repl.getHistoryViewer.getDocument
      // A cyclic buffer smaller than the history makes inserting the echo trim the document's
      // front, which shifts every offset the echo could have been recorded at.
      history.setCyclicBufferSize(history.getTextLength / 2)
      val historyBefore = history.getText
      setInput(repl, source)
      pressEnter(repl)

      val historyAfter = history.getText
      assertTrue(
        "the cyclic buffer must actually have trimmed the history for this test to mean anything",
        historyAfter.length < historyBefore.length
      )
      assertFalse(
        s"a submission that was never sent must not stay visible, got:\n$historyAfter",
        historyAfter.contains("val first")
      )
      assertTrue(
        s"only the echo may be removed, got:\n$historyAfter",
        historyBefore.endsWith(historyAfter)
      )
    }

  @Test
  def testBusyHintOffersAnExplicitResetBeforeSendingUserInput(): Unit =
    withStartedConsole { (repl, process) =>
      executePaste(repl, process, "val answer = readLine()\nprintln(answer)")
      val currentFile = loadedFile(process, 0)
      // The input echo itself looks exactly like a REPL prompt, and must not be mistaken for the
      // process prompt that ends the raw transport.
      val response = "scala> "

      setInput(repl, response)
      pressEnter(repl)

      assertEquals(response, repl.getEditorDocument.getText)
      assertFalse(
        s"the hint Enter must not send input, got: ${process.stdinChunks}",
        process.stdinText.contains(s"$response\n")
      )
      assertEquals("the hint Enter must not send a :load", 1, loadedFiles(process).size)
      assertFalse("the hint alone must not cancel healthy work", repl.isReadyForUserInput)
      assertTrue(remainingSubmissionFiles().nonEmpty)

      val notification = NotificationsManager.getNotificationsManager
        .getNotificationsOfType(classOf[Notification], getProject)
        .find(_.getTitle == busyTitle)
        .getOrElse(throw new AssertionError("expected a busy-input notification"))
      assertEquals(MyBundle.message("ui.repl.console.input.busy.message"), notification.getContent)
      val stopAction = notification.getActions.get(0).asInstanceOf[NotificationAction]
      assertEquals(MyBundle.message("ui.repl.console.input.busy.stop"), stopAction.getTemplateText)
      stopAction.actionPerformed(TestActionEvent.createTestEvent(), notification)

      assertTrue(repl.isReadyForUserInput)
      repl.flushDeferredText()
      assertTrue(
        repl.getHistoryViewer.getDocument.getText
          .contains(MyBundle.message("ui.repl.console.input.reset"))
      )
      assertTrue(
        "the current file must survive until the REPL acknowledges the command; the process " +
          "may have accepted the :load bytes without opening the file yet",
        Files.exists(currentFile)
      )
      assertEquals(
        "only the already-written current file may remain; queued files must be deleted",
        Seq(currentFile),
        remainingSubmissionFiles()
      )

      pressEnter(repl)
      assertTrue(process.stdinText.contains("scala> \n"))
      assertEquals("the first answer must not become another :load", 1, loadedFiles(process).size)
      assertEquals("", repl.getEditorDocument.getText)

      val secondResponse = "third line\nfourth line"
      setInput(repl, secondResponse)
      pressEnter(repl)
      assertTrue(
        "history's synthetic prompt must not end the raw transport before a process prompt",
        process.stdinText.contains("third line\nfourth line\n")
      )
      assertEquals("a second multiline answer must also stay raw", 1, loadedFiles(process).size)

      prompt(repl)
      assertFalse(
        "the acknowledging prompt must delete the abandoned current file",
        Files.exists(currentFile)
      )
      executePaste(repl, process, "var b = 1\nb = b + 1")
      assertEquals(
        "the next real prompt must restore the first split group",
        1,
        loadedFiles(process).size
      )
    }

  @Test
  def testDisposingAfterACancelledPasteDeletesTheAbandonedFile(): Unit =
    // Cancelling leaves the written group's file behind for the REPL to finish reading; disposal
    // is the last chance to delete it, as no prompt will acknowledge it any more.
    withStartedConsole { (repl, process) =>
      cancelPacedPaste(repl, process)
      assertTrue(remainingSubmissionFiles().nonEmpty)

      Disposer.dispose(repl)

      assertNoSubmissionFilesRemain()
    }

  @Test
  def testTerminationAfterACancelledPasteDeletesTheAbandonedFile(): Unit =
    withStartedConsole { (repl, process) =>
      cancelPacedPaste(repl, process)
      assertTrue(remainingSubmissionFiles().nonEmpty)

      process.terminate()

      assertNoSubmissionFilesRemain()
    }

  @Test
  def testRepeatedBusyEnterKeepsOneHintThatARestoredPromptExpires(): Unit =
    withStartedConsole { (repl, process) =>
      executePaste(repl, process, reassignmentPaste)
      pressEnter(repl)
      val firstHint = busyHints().head
      pressEnter(repl)

      // The first balloon may already have faded, so the second Enter posts a visible replacement
      // instead of being swallowed by the standing notification.
      assertTrue("the replaced hint must not stay retained", firstHint.isExpired)
      assertEquals("only the newest hint may be retained", 1, busyHints().size)

      prompt(repl)
      prompt(repl)
      prompt(repl)
      assertTrue(repl.isReadyForUserInput)
      assertTrue("restored readiness must expire the hint", busyHints().isEmpty)
    }

  @Test
  def testDisposingTheConsoleExpiresItsBusyHint(): Unit =
    withStartedConsole { (repl, process) =>
      executePaste(repl, process, reassignmentPaste)
      pressEnter(repl)
      assertEquals(1, busyHints().size)

      Disposer.dispose(repl)

      assertTrue("a disposed console must not keep its hint alive", busyHints().isEmpty)
    }

  @Test
  def testAnOldBusyNotificationCannotCancelANewerPaste(): Unit =
    withStartedConsole { (repl, process) =>
      executePaste(repl, process, "val first = readLine()\nprintln(first)")
      pressEnter(repl)
      val oldNotification = NotificationsManager.getNotificationsManager
        .getNotificationsOfType(classOf[Notification], getProject)
        .filter(_.getTitle == busyTitle)
        .last
      val oldStopAction = oldNotification.getActions.get(0).asInstanceOf[NotificationAction]

      prompt(repl)
      prompt(repl)
      assertTrue(repl.isReadyForUserInput)

      executePaste(repl, process, "val second = readLine()\nprintln(second)")
      val newFirstFile = loadedFile(process, 0)
      oldStopAction.actionPerformed(TestActionEvent.createTestEvent(), oldNotification)

      assertFalse("an action for an older paste must not cancel this one", repl.isReadyForUserInput)
      assertTrue("the newer paste's current file must remain", Files.exists(newFirstFile))
      prompt(repl)
      prompt(repl)
      assertTrue(repl.isReadyForUserInput)
    }

  @Test
  def testBusyNotificationCanCancelItsPasteAfterThatPasteAdvances(): Unit =
    withStartedConsole { (repl, process) =>
      executePaste(repl, process, reassignmentPaste)
      pressEnter(repl)
      val notification = NotificationsManager.getNotificationsManager
        .getNotificationsOfType(classOf[Notification], getProject)
        .filter(_.getTitle == busyTitle)
        .last
      val stopAction = notification.getActions.get(0).asInstanceOf[NotificationAction]

      prompt(repl)
      assertEquals(2, loadedFiles(process).size)
      val currentFile = loadedFile(process, 1)
      stopAction.actionPerformed(TestActionEvent.createTestEvent(), notification)

      assertTrue("the action must remain valid for the same paste", repl.isReadyForUserInput)
      assertEquals(Seq(currentFile), remainingSubmissionFiles())
      prompt(repl)
      assertNoSubmissionFilesRemain()
    }

  @Test
  def testLeavesEditorAndHistoryUntouchedWhenSubmissionPreparationFails(): Unit =
    withConsole(startupComplete = true, failPreparationAt = 1) { (repl, process) =>
      val source = "val first = 1\nval second = 2"
      executePaste(repl, process, source)

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

  @Test
  def testDeletesEarlierFilesWhenALaterPreparationFails(): Unit =
    // A partial preparation failure must not leave the already-created files of the earlier
    // groups behind: the console never learns about them, so nothing else could delete them.
    withConsole(startupComplete = true, failPreparationAt = 2) { (repl, process) =>
      executePaste(repl, process, reassignmentPaste)

      assertEquals(reassignmentPaste, repl.getEditorDocument.getText)
      assertFalse(
        "no :load command may be sent when preparation fails",
        process.stdinChunks.exists(_.contains(":load"))
      )
      assertNoSubmissionFilesRemain("the earlier group's file must be deleted, still present")
      assertTrue(repl.isReadyForUserInput)
    }

  @Test
  def testSendsAnUnparseablePasteAsOneWholeLoad(): Unit =
    // The splitter reports no chunks for input it cannot split; the paste must still go out
    // verbatim, so that the REPL reports the error the user pasted.
    withStartedConsole { (repl, process) =>
      val source     = "val x = (1 +\ndef"
      val sourceFile = executeMultiline(repl, process, source)

      assertEquals(source, Files.readString(sourceFile, UTF_8))
      // A prompt sends the next queued group if there is one; nothing may be queued behind it.
      prompt(repl)
      assertEquals("the whole paste must go out as one :load", 1, loadedFiles(process).size)
    }

  @Test
  def testFallsBackToTheWholePasteWhenTheSplitterFails(): Unit =
    // A splitter failure must degrade to the pre-splitting behavior, one :load with the whole
    // paste, never to a swallowed Enter.
    withConsole(startupComplete = true, failParserFeatures = true) { (repl, process) =>
      val sourceFile = executeMultiline(repl, process, reassignmentPaste)

      assertEquals(reassignmentPaste, Files.readString(sourceFile, UTF_8))
      // A prompt sends the next queued group if there is one; the fallback must leave nothing
      // queued behind the whole-paste :load.
      prompt(repl)
      assertEquals("the whole paste must go out as one :load", 1, loadedFiles(process).size)
      assertNoSubmissionFilesRemain()
    }

  @Test
  def testSingleLineInputBypassesTheMultilinePath(): Unit =
    withStartedConsole { (repl, process) =>
      executePaste(repl, process, "1 + 1")

      assertFalse(
        "single-line input must go through the stock action, not :load",
        process.stdinChunks.exists(_.contains(":load"))
      )
      assertEquals("", repl.getEditorDocument.getText)
    }

  @Test
  def testPacesSemicolonSeparatedStatementsOnOneLine(): Unit =
    // Sent as one unit these three statements collide in a single REPL wrapper object, so one line
    // of them must be paced exactly like the same statements on three lines.
    withStartedConsole { (repl, process) =>
      executePaste(repl, process, "var a = 1; a = a + 1; a = a + 1")

      assertEquals("var a = 1", Files.readString(loadedFile(process, 0), UTF_8).trim)
      assertFalse(repl.isReadyForUserInput)

      prompt(repl)
      assertEquals("a = a + 1", Files.readString(loadedFile(process, 1), UTF_8).trim)
      assertFalse(repl.isReadyForUserInput)

      prompt(repl)
      assertEquals("a = a + 1", Files.readString(loadedFile(process, 2), UTF_8).trim)
      assertFalse(repl.isReadyForUserInput)

      prompt(repl)
      repl.flushDeferredText()
      assertTrue(repl.isReadyForUserInput)
      val history = repl.getHistoryViewer.getDocument.getText
      assertTrue(s"the console must end at a prompt, got:\n$history", history.endsWith("scala> "))
    }

  @Test
  def testSendsASemicolonInsideAStringAsOneSubmission(): Unit =
    // The gate only looks for a `;`, so the splitter has to be what decides that this line is a
    // single statement.
    withStartedConsole { (repl, process) =>
      val source = "val s = \"a;b\""
      assertEquals(source, Files.readString(executeMultiline(repl, process, source), UTF_8))

      // A prompt sends the next queued group if there is one; nothing may be queued behind it.
      prompt(repl)
      assertEquals("the line must go out as one :load", 1, loadedFiles(process).size)
    }

  @Test
  def testSequentialSingleLinesStaySeparatedInTheConsoleContext(): Unit =
    withStartedConsole { (repl, process) =>
      executePaste(repl, process, "val first = 1")
      executePaste(repl, process, "println(first)")

      assertEquals("val first = 1\nprintln(first)\n", repl.getHistory)
    }

  @Test
  def testAPasteAfterASingleLineGoesRawUntilTheNextPrompt(): Unit =
    // A raw line can leave the REPL mid-input, on a continuation prompt this plugin does not
    // recognize, and a :load written into that pending input would be parsed as source text.
    withStartedConsole { (repl, process) =>
      executePaste(repl, process, "println(1 + 1")
      assertTrue("the user must still be able to finish the expression", repl.isReadyForUserInput)

      process.clearInput()
      setInput(repl, reassignmentPaste)
      pressEnter(repl)

      assertTrue(
        s"no :load may reach a REPL that is mid-input, got: ${loadedFiles(process)}",
        loadedFiles(process).isEmpty
      )
      assertTrue(
        s"the raw paste must go out line by line, got: ${process.stdinChunks}",
        process.stdinChunks.containsSlice(List("var a = 1\n", "a = a + 1\n", "a = a + 1\n"))
      )
      assertEquals("a sent paste must leave the editor", "", repl.getEditorDocument.getText)
      assertFalse(
        "input that is sent raw must not report the console as unavailable",
        unavailableHints().nonEmpty
      )

      // The prompt says the REPL is back at top level, so split pacing resumes.
      prompt(repl)
      executePaste(repl, process, reassignmentPaste)
      assertEquals("var a = 1", Files.readString(loadedFile(process, 0), UTF_8).trim)
      assertFalse(repl.isReadyForUserInput)
    }

  @Test
  def testAScala2ConsoleSendsMultilineInputRaw(): Unit =
    // Only the Scala 3 console paces its input against prompts, so a :load into a Scala 2 REPL
    // would have nothing to fall back on. Its lines must reach the process as typed.
    withConsole(startupComplete = true, scala3 = false) { (repl, process) =>
      executePaste(repl, process, "val a = 1\nval b = 2")

      assertTrue(
        s"no :load may reach a Scala 2 REPL, got: ${process.stdinChunks}",
        loadedFiles(process).isEmpty
      )
      assertTrue(
        s"the paste must go out line by line, got: ${process.stdinChunks}",
        process.stdinChunks.containsSlice(List("val a = 1\n", "val b = 2\n"))
      )
    }

  @Test
  def testMultilineAndFollowingInputStaySeparatedInTheConsoleContext(): Unit =
    withStartedConsole { (repl, process) =>
      executePaste(repl, process, reassignmentPaste)
      prompt(repl)
      prompt(repl)
      prompt(repl)
      executePaste(repl, process, "println(a)")

      assertEquals(s"$reassignmentPaste\nprintln(a)\n", repl.getHistory)
    }

  @Test
  def testSendSelectionCannotWriteIntoAnActivePaste(): Unit =
    withStartedConsole { (repl, process) =>
      executePaste(repl, process, "val answer = readLine()\nprintln(answer)")
      val selection = "println(9)"
      val factory   = EditorFactory.getInstance
      val editor    = factory.createEditor(factory.createDocument(selection), getProject)
      try
        editor.getSelectionModel.setSelection(0, editor.getDocument.getTextLength)
        val event = TestActionEvent.createTestEvent(
          SimpleDataContext
            .builder()
            .add(CommonDataKeys.PROJECT, getProject)
            .add(CommonDataKeys.EDITOR, editor)
            .build()
        )

        new SendSelectionToConsoleAction().actionPerformed(event)

        assertFalse(
          s"selection input must not enter the active paste, got: ${process.stdinChunks}",
          process.stdinText.contains(selection)
        )
        assertEquals("the selection must not become a :load", 1, loadedFiles(process).size)
        assertTrue(busyHints().nonEmpty)
      finally factory.releaseEditor(editor)
    }

  @Test
  def testMultilineSelectionUsesSplitPacing(): Unit =
    withStartedConsole { (repl, process) =>
      val factory = EditorFactory.getInstance
      val editor  = factory.createEditor(factory.createDocument(reassignmentPaste), getProject)
      try
        editor.getSelectionModel.setSelection(0, reassignmentPaste.length)
        process.clearInput()
        val context = SimpleDataContext
          .builder()
          .add(CommonDataKeys.PROJECT, getProject)
          .add(CommonDataKeys.EDITOR, editor)
          .build()

        new SendSelectionToConsoleAction().actionPerformed(
          TestActionEvent.createTestEvent(context)
        )

        assertEquals("only the first selected group may be sent", 1, loadedFiles(process).size)
        assertEquals("var a = 1", Files.readString(loadedFile(process, 0), UTF_8).trim)
        assertFalse(repl.isReadyForUserInput)
      finally factory.releaseEditor(editor)
    }

  @Test
  def testSendSelectionGoesToTheConsoleOfTheSelectedFilesModule(): Unit =
    // The Scala plugin's registry answers per project with the console started last, and REPLace
    // makes a second console easy to start, so the selection must be routed by its own module.
    withStartedConsole { (repl, process) =>
      withSecondConsole { otherProcess =>
        val file    = createFileInTheModule("Selection.scala")
        val factory = EditorFactory.getInstance
        val editor  = factory.createEditor(FileDocumentManager.getInstance.getDocument(file))
        try
          inWriteAction(editor.getDocument.setText("1 + 1"))
          editor.getSelectionModel.setSelection(0, editor.getDocument.getTextLength)
          process.clearInput()
          val context = SimpleDataContext
            .builder()
            .add(CommonDataKeys.PROJECT, getProject)
            .add(CommonDataKeys.EDITOR, editor)
            .build()

          new SendSelectionToConsoleAction().actionPerformed(
            TestActionEvent.createTestEvent(context)
          )

          assertTrue(process.stdinText, process.stdinText.contains("1 + 1\n"))
          assertEquals("the newest console is not the one to run it", "", otherProcess.stdinText)
        finally factory.releaseEditor(editor)
      }
    }

  @Test
  def testSendSelectionPrefersALiveConsoleOfTheModule(): Unit =
    // A stopped console can outlive its process in a pinned run tab, and it must not swallow a
    // selection meant for the module's usable REPL.
    withStoppedConsole {
      withStartedConsole { (repl, process) =>
        val file    = createFileInTheModule("Preferred.scala")
        val factory = EditorFactory.getInstance
        val editor  = factory.createEditor(FileDocumentManager.getInstance.getDocument(file))
        try
          inWriteAction(editor.getDocument.setText("1 + 1"))
          editor.getSelectionModel.setSelection(0, editor.getDocument.getTextLength)
          process.clearInput()
          val context = SimpleDataContext
            .builder()
            .add(CommonDataKeys.PROJECT, getProject)
            .add(CommonDataKeys.EDITOR, editor)
            .build()

          new SendSelectionToConsoleAction().actionPerformed(
            TestActionEvent.createTestEvent(context)
          )

          assertTrue(process.stdinText, process.stdinText.contains("1 + 1\n"))
        finally factory.releaseEditor(editor)
      }
    }

  @Test
  def testAHintForAConsoleThatIsAlreadyReadyExpiresItself(): Unit =
    // The output pump can restore readiness between posting the hint and storing it, and its own
    // expiry then finds nothing to expire, leaving a stale balloon over a usable console.
    withStartedConsole { (repl, _) =>
      assertTrue(repl.isReadyForUserInput)

      repl.showInputUnavailableHint()

      assertTrue("a hint for a ready console must not stay standing", unavailableHints().isEmpty)
    }

  @Test
  def testSendSelectionWithoutASelectionSendsNothing(): Unit =
    // The stock action dereferences the selection and would throw on a caret-only editor.
    withStartedConsole { (repl, process) =>
      val factory = EditorFactory.getInstance
      val editor  = factory.createEditor(factory.createDocument("val unselected = 1"), getProject)
      try
        process.clearInput()
        val context = SimpleDataContext
          .builder()
          .add(CommonDataKeys.PROJECT, getProject)
          .add(CommonDataKeys.EDITOR, editor)
          .build()

        new SendSelectionToConsoleAction().actionPerformed(
          TestActionEvent.createTestEvent(context)
        )

        assertEquals("", process.stdinText)
        assertEquals("the console input must stay untouched", "", repl.getEditorDocument.getText)
      finally factory.releaseEditor(editor)
    }

  @Test
  def testTheStockUpdateDisablesEnterAfterProcessTermination(): Unit =
    withStartedConsole { (repl, process) =>
      process.terminate()
      val event = TestActionEvent.createTestEvent(
        SimpleDataContext.getSimpleContext(CommonDataKeys.EDITOR, repl.getConsoleEditor)
      )
      new ConsoleExecuteAction().update(event)
      assertFalse(event.getPresentation.isEnabled)
    }

  @Test
  def testDoesNothingForAnEditorThatIsNotAConsole(): Unit =
    val factory = EditorFactory.getInstance
    val editor  = factory.createEditor(factory.createDocument("object NotAConsole"), getProject)
    try
      val context = SimpleDataContext.getSimpleContext(CommonDataKeys.EDITOR, editor)
      // Must not throw even though no Scala console is attached to the editor.
      new ConsoleExecuteAction().actionPerformed(TestActionEvent.createTestEvent(context))
    finally factory.releaseEditor(editor)

  /** Arms the write hook so that `action` runs on a real output thread while the next group's
    * `:load` is still being written, the window where it is published but not recorded as sent. The
    * returned flag says whether that thread was still running when the writer moved on; the
    * production code swallows every NonFatal, an AssertionError included, so the hook itself cannot
    * assert.
    */
  private def duringNextGroupWrite(process: RecordingProcessHandler)(
      action: () => Unit
  ): AtomicBoolean =
    val blocked = new AtomicBoolean(false)
    process.afterBytesAccepted = payload =>
      if payload.startsWith(":load ") then
        process.afterBytesAccepted = _ => ()
        val thread = new Thread(() => action(), "test-repl-output")
        thread.start()
        thread.join(5000)
        blocked.set(thread.isAlive)
    blocked

  private def prompt(repl: Repl): Unit =
    repl.print("scala> ", promptOutputType)

  /** Arms the write hook to kill the process from inside the next `:load` write, once. */
  private def terminateDuringNextWrite(process: RecordingProcessHandler): Unit =
    process.onWrite = payload =>
      if payload.startsWith(":load ") then
        process.onWrite = _ => ()
        process.terminate()

  /** Arms the write hook to kill the process after the bytes reached its input stream, once. */
  private def terminateAfterBytesAreAccepted(process: RecordingProcessHandler): Unit =
    process.afterBytesAccepted = payload =>
      if payload.startsWith(":load ") then
        process.afterBytesAccepted = _ => ()
        process.terminate()

  /** Starts a paste that blocks in `readLine` and cancels its pacing from the busy hint, which
    * abandons the file of the group the REPL is still reading.
    */
  private def cancelPacedPaste(repl: Repl, process: RecordingProcessHandler): Unit =
    executePaste(repl, process, "val answer = readLine()\nprintln(answer)")
    setInput(repl, "an answer")
    pressEnter(repl)
    val hint = busyHints().last
    val stop = hint.getActions.get(0).asInstanceOf[NotificationAction]
    stop.actionPerformed(TestActionEvent.createTestEvent(), hint)

  /** Types `source` into the console editor and presses Enter. */
  private def executePaste(repl: Repl, process: RecordingProcessHandler, source: String): Unit =
    process.clearInput()
    setInput(repl, source)
    pressEnter(repl)

  /** Types `source` into the console editor without submitting it. */
  private def setInput(repl: Repl, source: String): Unit =
    inWriteAction(repl.getEditorDocument.setText(source))

  private def pressEnter(repl: Repl): Unit =
    val context = SimpleDataContext.getSimpleContext(CommonDataKeys.EDITOR, repl.getConsoleEditor)
    new ConsoleExecuteAction().actionPerformed(TestActionEvent.createTestEvent(context))

  private def executeMultiline(repl: Repl, process: RecordingProcessHandler, source: String): Path =
    executePaste(repl, process, source)
    loadedFile(process, 0)

  /** The `index`-th (0-based) file `:load`-ed so far, asserting that exactly `index + 1` were. */
  private def loadedFile(process: RecordingProcessHandler, index: Int): Path =
    val files = loadedFiles(process)
    assertEquals(s"expected ${index + 1} :load(s), got: $files", index + 1, files.size)
    files(index)

  /** The history after the first echo, which the startup and echo prompts precede. */
  private def outputRegion(history: String): String =
    history.substring(history.indexOf(reassignmentEchoes.head))

  private def loadedFiles(process: RecordingProcessHandler): List[Path] =
    process.stdinChunks
      .filter(_.startsWith(":load "))
      .map(chunk => Path.of(chunk.stripSuffix("\n").stripPrefix(":load ")))
      .toList

  /** The busy-input hints the platform still holds for this project. */
  private def busyHints(): Seq[Notification] =
    NotificationsManager.getNotificationsManager
      .getNotificationsOfType(classOf[Notification], getProject)
      .filter(_.getTitle == busyTitle)
      .toSeq

  /** The hints posted for an Enter that could not be sent at all. */
  private def unavailableHints(): Seq[Notification] =
    NotificationsManager.getNotificationsManager
      .getNotificationsOfType(classOf[Notification], getProject)
      .filter(_.getTitle == MyBundle.message("ui.repl.console.unavailable.title"))
      .toSeq

  /** A file under the module's content root, which is what resolves it to that module. */
  private def createFileInTheModule(name: String): VirtualFile =
    val root = ModuleRootManager.getInstance(getModule).getContentRoots.head
    WriteAction.compute[VirtualFile, IOException](() => root.createChildData(this, name))

  /** Runs `test` with an older, stopped console of the same module already open. */
  private def withStoppedConsole(test: => Unit): Unit =
    val console = new Repl(getModule):
      override private[replace] def initialCommandsFile: Option[Path] = None
      override private[replace] def scala3Module: Boolean             = true
    withRepl(console) { (_, process) =>
      process.startNotify()
      process.terminate()
      test
    }

  /** Runs `test` with a second, newer console open for a module of its own. */
  private def withSecondConsole(test: RecordingProcessHandler => Unit): Unit =
    val console = new Repl(createSecondModule()):
      override private[replace] def initialCommandsFile: Option[Path] = None
      override private[replace] def scala3Module: Boolean             = true
    withRepl(console) { (repl, process) =>
      process.startNotify()
      repl.print(Repl.welcomeLine, ConsoleViewContentType.NORMAL_OUTPUT)
      repl.print("scala> ", ConsoleViewContentType.NORMAL_OUTPUT)
      test(process)
    }

  private def remainingSubmissionFiles(): Seq[Path] =
    createdSubmissionFiles.toSeq.filter(Files.exists(_))

  private def assertNoSubmissionFilesRemain(
      prefix: String = "every created file must be deleted, still present"
  ): Unit =
    val remaining = remainingSubmissionFiles()
    assertTrue(s"$prefix: $remaining", remaining.isEmpty)

  private def withStartedConsole(test: (Repl, RecordingProcessHandler) => Unit): Unit =
    withConsole(startupComplete = true)(test)

  private def withConsole(
      startupComplete: Boolean,
      failPreparationAt: Int = -1,
      failParserFeatures: Boolean = false,
      startupPromptType: ConsoleViewContentType = ConsoleViewContentType.NORMAL_OUTPUT,
      scala3: Boolean = true
  )(
      test: (Repl, RecordingProcessHandler) => Unit
  ): Unit =
    createdSubmissionFiles.clear()
    var preparations = 0
    val console = new Repl(getModule):
      override private[replace] def initialCommandsFile: Option[Path] = None
      override private[replace] def scala3Module: Boolean             = scala3
      // The light test module has no Scala SDK to derive features from.
      override def parserFeatures: ScalaFeatures =
        if failParserFeatures then throw new IllegalStateException("simulated splitter failure")
        else ScalaFeatures.forParserTests(LatestScalaVersions.Scala_3_9)
      override def prepareMultilineSubmission(
          source: String,
          lineOffset: Int
      ): Scala3MultilineSubmission =
        preparations += 1
        if preparations == failPreparationAt then
          throw new IOException("simulated temporary file failure")
        val submission = super.prepareMultilineSubmission(source, lineOffset)
        createdSubmissionFiles += submission.sourceFile
        submission
    withRepl(console) { (repl, process) =>
      process.startNotify()
      if startupComplete then
        repl.print(Repl.welcomeLine, ConsoleViewContentType.NORMAL_OUTPUT)
        repl.print("scala> ", startupPromptType)
      test(repl, process)
    }
