// This file uses modified code based on the IntelliJ Scala plugin.
// Original code can be found here:
// https://github.com/JetBrains/intellij-scala/blob/bd2ec19ced511fd2f27459ca733dde5cb432aba6/scala/scala-impl/src/org/jetbrains/plugins/scala/console/actions/ScalaConsoleExecuteAction.scala

// The reason for this class being in a separate package is that the ConsoleExecuteAction class
// uses the ScalaLanguageConsole.textSent() method, which is package private.
// Therefore, to call it, we must be in the same package as the console: org.jetbrains.plugins.scala.console.

package org.jetbrains.plugins.scala.console.replace

import com.intellij.execution.console.ConsoleHistoryController
import com.intellij.execution.process.ProcessHandler
import com.intellij.openapi.actionSystem.{ActionUpdateThread, AnActionEvent, CommonDataKeys}
import com.intellij.openapi.application.ApplicationManager
import com.intellij.openapi.diagnostic.Logger
import com.intellij.openapi.editor.{Document, Editor}
import com.intellij.openapi.util.TextRange
import fi.aalto.cs.replace.Repl
import org.jetbrains.plugins.scala.console.ScalaConsoleInfo
import org.jetbrains.plugins.scala.console.actions.ScalaConsoleExecuteAction
import org.jetbrains.plugins.scala.extensions.inWriteAction

import scala.util.control.NonFatal

class ConsoleExecuteAction extends ScalaConsoleExecuteAction:

  override def actionPerformed(e: AnActionEvent): Unit =
    // Any of these can already be absent while the console is being disposed.
    val consoleContext = for
      editor            <- Option(e.getData(CommonDataKeys.EDITOR))
      console           <- Option(ScalaConsoleInfo.getConsole(editor))
      processHandler    <- Option(ScalaConsoleInfo.getProcessHandler(editor))
      historyController <- Option(ScalaConsoleInfo.getController(editor))
    yield (editor, console, processHandler, historyController)

    consoleContext match
      case None                                                       => ()
      case Some((editor, console, processHandler, historyController)) =>
        // Multiline submissions are only rewritten for our custom REPL running Scala 3; Scala 2
        // and single-line input go through the stock Scala plugin action.
        console match
          case repl: Repl if !repl.isReadyForUserInput => ()
          case repl: Repl
              if repl.isScala3REPL && repl.getEditorDocument.getText
                .exists(c => c == '\n' || c == '\r') =>
            submitMultiline(repl, processHandler, historyController, editor)
          case _ => super.actionPerformed(e)

  private def submitMultiline(
      repl: Repl,
      processHandler: ProcessHandler,
      historyController: ConsoleHistoryController,
      editor: Editor
  ): Unit =
    val document = repl.getEditorDocument
    val text     = document.getText

    // The submission file is prepared before history is touched: if it cannot be created, the
    // editor and history are left exactly as they were.
    val prepared = for
      outputStream <- Option(processHandler.getProcessInput)
      submission <-
        try Some(repl.prepareMultilineSubmission(text))
        catch
          case NonFatal(error) =>
            ConsoleExecuteAction.logger.warn("Could not submit multiline Scala 3 REPL input", error)
            None
    yield (outputStream, submission)

    prepared.foreach { (outputStream, submission) =>
      // The Scala console expects history to be updated before anything is written to the process.
      ApplicationManager.getApplication.runWriteAction(
        new Runnable():
          def run(): Unit =
            val range: TextRange = new TextRange(0, document.getTextLength)
            editor.getSelectionModel.setSelection(range.getStartOffset, range.getEndOffset)
            // note: it uses `range` instead ot just editor `text` because under the hood it splits actual editor content
            // according to the highlighter attributes and passes the correct ContentType to the history console
            repl.addToHistory(range, repl.getConsoleEditor, true)
            // without this line there will be a slight blinking of user input code SCL-16655
            // see com.intellij.execution.impl.ConsoleViewImpl.print
            repl.flushDeferredText()
            historyController.addToHistory(text)

            editor.getCaretModel.moveToOffset(0)
            editor.getDocument.setText("")
      )

      val sent =
        try
          repl.sendMultilineSubmission(submission, outputStream)
          true
        catch
          case NonFatal(error) =>
            ConsoleExecuteAction.logger.warn("Could not submit multiline Scala 3 REPL input", error)
            restoreEditorText(document, text)
            false

      if sent then repl.textSent(text)
    }

  private def restoreEditorText(document: Document, text: String): Unit =
    ApplicationManager.getApplication.runWriteAction(
      new Runnable():
        def run(): Unit =
          if document.getTextLength == 0 then document.setText(text)
    )

  override def update(e: AnActionEvent): Unit =
    super.update(e)
    if e.getPresentation.isEnabled then
      Option(e.getData(CommonDataKeys.EDITOR))
        .flatMap(editor => Option(ScalaConsoleInfo.getConsole(editor)))
        .foreach {
          case repl: Repl => e.getPresentation.setEnabled(repl.isReadyForUserInput)
          case _          => ()
        }

  override def getActionUpdateThread: ActionUpdateThread = ActionUpdateThread.BGT

object ConsoleExecuteAction:
  private val logger = Logger.getInstance(classOf[ConsoleExecuteAction])
