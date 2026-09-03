// This file uses modified code based on the IntelliJ Scala plugin.
// Original code can be found here:
// https://github.com/JetBrains/intellij-scala/blob/2026.2.15/scala/repl/src/org/jetbrains/plugins/scala/console/actions/ScalaConsoleExecuteAction.scala

// This class calls ScalaLanguageConsole.textSent(), which is private[console] and therefore
// visible to this subpackage of org.jetbrains.plugins.scala.console.

package org.jetbrains.plugins.scala.console.replace

import com.intellij.execution.console.ConsoleHistoryController
import com.intellij.execution.process.ProcessHandler
import com.intellij.openapi.actionSystem.{AnActionEvent, CommonDataKeys}
import com.intellij.openapi.diagnostic.Logger
import com.intellij.openapi.editor.{CaretState, Document, Editor, RangeMarker}
import com.intellij.openapi.progress.ProcessCanceledException
import com.intellij.openapi.util.TextRange
import com.intellij.util.concurrency.ThreadingAssertions
import com.intellij.util.concurrency.annotations.RequiresEdt
import fi.aalto.cs.replace.{Repl, Scala3MultilineSubmission, Scala3StatementSplitter}
import org.jetbrains.plugins.scala.console.ScalaConsoleInfo
import org.jetbrains.plugins.scala.console.actions.ScalaConsoleExecuteAction
import org.jetbrains.plugins.scala.extensions.inWriteAction

import java.io.IOException
import scala.util.control.NonFatal

class ConsoleExecuteAction extends ScalaConsoleExecuteAction:

  override def actionPerformed(e: AnActionEvent): Unit =
    // Everything here can already be absent while the console is being disposed.
    val editor  = e.getData(CommonDataKeys.EDITOR)
    val console = if editor == null then null else ScalaConsoleInfo.getConsole(editor)
    console match
      case null       => ()
      case repl: Repl =>
        // The console need not have had focus since the last edit, so its banner can be stale.
        repl.refreshChangeBanner()
        if !repl.isReadyForUserInput then repl.showInputUnavailableHint()
        else
          val processHandler    = ScalaConsoleInfo.getProcessHandler(editor)
          val historyController = ScalaConsoleInfo.getController(editor)
          if processHandler != null && historyController != null then
            val text           = repl.getEditorDocument.getText
            val multiStatement = text.contains('\n') || text.contains(';')
            if repl.isScala3REPL && !repl.mustUseRawTransport && multiStatement then
              submitMultiline(repl, processHandler, historyController, editor, text)
            else submitNormally(repl, processHandler, historyController, editor, text)
      case _ => super.actionPerformed(e)

  /** The stock single-line submission path, reproduced so that the echo goes through
    * [[Repl.addInputToHistory]] and is not mistaken for process output.
    */
  @RequiresEdt
  private def submitNormally(
      console: Repl,
      processHandler: ProcessHandler,
      historyController: ConsoleHistoryController,
      editor: Editor,
      text: String
  ): Unit =
    ThreadingAssertions.assertEventDispatchThread()
    Option(processHandler.getProcessInput).foreach { outputStream =>
      val document = console.getEditorDocument
      // Before the write: a prompt answering these lines can reach the output pump while this loop
      // is still running, and it must be free to end the raw transport this write starts.
      console.useRawTransportUntilPrompt()
      var written = true
      text.split('\n').foreach { line =>
        val lineToSend = s"$line\n"
        try
          // Match the stock Scala console action, including its use of the JVM default charset.
          outputStream.write(lineToSend.getBytes())
          outputStream.flush()
        catch
          case error: IOException =>
            written = false
            ConsoleExecuteAction.logger.warn(
              s"Could not send Scala console input '${line.take(1000)}'",
              error
            )
        console.textSent(lineToSend)
      }
      // The echo and the history entry follow the write, because a submission the REPL never saw
      // must leave the user's text in place, and rolling back an echo would delete later output.
      if written then
        inWriteAction {
          val range = new TextRange(0, document.getTextLength)
          editor.getSelectionModel.setSelection(range.getStartOffset, range.getEndOffset)
          console.addInputToHistory(range)
          console.flushDeferredText()
          historyController.addToHistory(text)
          editor.getCaretModel.moveToOffset(0)
          document.setText("")
        }
    }

  /** The Scala 3 `:load` path: the paste is split into statement groups, echoed, and then fed to
    * the REPL one group per prompt. A paste that never reaches the REPL is rolled back.
    */
  @RequiresEdt
  private def submitMultiline(
      repl: Repl,
      processHandler: ProcessHandler,
      historyController: ConsoleHistoryController,
      editor: Editor,
      text: String
  ): Unit =
    ThreadingAssertions.assertEventDispatchThread()
    val document = repl.getEditorDocument
    // Captured before the editor is cleared, so a failed submission can restore the carets.
    val caretsAndSelections = editor.getCaretModel.getCaretsAndSelections

    // Prepared before history is touched, so a preparation failure changes nothing.
    val prepared = for
      outputStream <- Option(processHandler.getProcessInput)
      submissions <-
        try Some(prepareAll(repl, splitSources(repl, text)))
        catch
          // Cancellation must propagate, because swallowing it leaves the action doing nothing.
          case cancellation: ProcessCanceledException => throw cancellation
          case NonFatal(error) =>
            ConsoleExecuteAction.logger.warn("Could not submit multiline Scala 3 REPL input", error)
            None
    yield (outputStream, submissions)

    prepared.foreach { (outputStream, submissions) =>
      // The visible echo is added before anything is written to the process. Navigable history is
      // added only after the first write succeeds, so a failed submission leaves no false entry.
      val echoMarker =
        try echoInput(repl, editor, document)
        catch
          case error: Throwable =>
            // The console does not know about the submissions yet, so nothing else deletes them.
            submissions.foreach(_.close())
            throw error

      try
        val sent =
          try repl.sendMultilineSubmissions(submissions, outputStream)
          catch
            case cancellation: ProcessCanceledException =>
              // The paste never reached the REPL, so a cancel must not also lose it.
              restoreFailedSubmission(repl, editor, text, caretsAndSelections, echoMarker)
              throw cancellation
            case NonFatal(error) =>
              ConsoleExecuteAction.logger
                .warn("Could not submit multiline Scala 3 REPL input", error)
              false

        if sent then
          historyController.addToHistory(text)
          repl.textSent(ConsoleExecuteAction.withTrailingNewline(text))
        else restoreFailedSubmission(repl, editor, text, caretsAndSelections, echoMarker)
      finally echoMarker.dispose()
    }

  /** The statement groups the paste splits into, or the whole paste verbatim when it is a single
    * group or cannot be split, because a splitter failure must not swallow the user's Enter.
    */
  private def splitSources(repl: Repl, text: String): List[Scala3StatementSplitter.Chunk] =
    val chunks =
      try Scala3StatementSplitter.splitWithLineOffsets(text, repl.parserFeatures, repl.getProject)
      catch
        case cancellation: ProcessCanceledException => throw cancellation
        case NonFatal(error) =>
          ConsoleExecuteAction.logger.warn("Could not split multiline REPL input", error)
          Nil
    // A reconstructed single chunk would drop the trailing newline, so it is sent verbatim.
    if chunks.sizeIs > 1 then chunks
    else List(Scala3StatementSplitter.Chunk(text, lineOffset = 0))

  /** Prepares one submission per chunk, deleting the earlier files if a later one fails. */
  private def prepareAll(
      repl: Repl,
      chunks: List[Scala3StatementSplitter.Chunk]
  ): List[Scala3MultilineSubmission] =
    chunks
      .foldLeft(List.empty[Scala3MultilineSubmission]) { (prepared, chunk) =>
        try repl.prepareMultilineSubmission(chunk.source, chunk.lineOffset) :: prepared
        catch
          case error: Throwable =>
            prepared.foreach(_.close())
            throw error
      }
      .reverse

  /** Copies the input into the history, clears the editor and returns a marker over the echo, so a
    * submission that never reaches the REPL can be rolled back.
    */
  @RequiresEdt
  private def echoInput(repl: Repl, editor: Editor, document: Document): RangeMarker =
    inWriteAction {
      // Keep output that predates this action outside the rollback range.
      repl.flushDeferredText()
      val historyDocument = repl.getHistoryViewer.getDocument
      // The history document is a cyclic buffer, so inserting the echo can trim its front and
      // shift every offset in it. A marker tracks the echo across such a trim.
      val marker = historyDocument
        .createRangeMarker(historyDocument.getTextLength, historyDocument.getTextLength)
      marker.setGreedyToRight(true)
      val range = new TextRange(0, document.getTextLength)
      editor.getSelectionModel.setSelection(range.getStartOffset, range.getEndOffset)
      // `range` lets the copy split content by highlighter attributes for the ContentType.
      repl.addInputToHistory(range)
      // Without this flush the user input blinks (SCL-16655, see ConsoleViewImpl.print).
      repl.flushDeferredText()

      editor.getCaretModel.moveToOffset(0)
      editor.getDocument.setText("")
      marker
    }

  @RequiresEdt
  private def restoreFailedSubmission(
      repl: Repl,
      editor: Editor,
      text: String,
      caretsAndSelections: java.util.List[CaretState],
      echoMarker: RangeMarker
  ): Unit =
    ThreadingAssertions.assertEventDispatchThread()
    inWriteAction {
      if echoMarker.isValid then
        repl.getHistoryViewer.getDocument
          .deleteString(echoMarker.getStartOffset, echoMarker.getEndOffset)
      val document = editor.getDocument
      if document.getTextLength == 0 then document.setText(text)
      editor.getCaretModel.setCaretsAndSelections(caretsAndSelections)
    }

object ConsoleExecuteAction:
  private val logger = Logger.getInstance(classOf[ConsoleExecuteAction])

  private def withTrailingNewline(text: String): String =
    if text.endsWith("\n") then text else text + "\n"
