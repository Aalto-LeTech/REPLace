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
import com.intellij.openapi.editor.Editor
import com.intellij.openapi.util.TextRange
import fi.aalto.cs.replace.Repl
import org.jetbrains.plugins.scala.console.ScalaConsoleInfo
import org.jetbrains.plugins.scala.console.actions.ScalaConsoleExecuteAction

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
        // We should perform our multiline fixing only for our custom REPL that is running Scala 3.
        // REPLs that host Scala 2 should not be modified.
        // Additionally, if the text has no newlines, we don't need to modify it.
        console match
          // Startup commands are still being fed to the REPL; accepting input now would interleave
          // it with them.
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

    // Process input and add to history
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

    val outputStream = processHandler.getProcessInput
    if outputStream != null then
      // Empty lines end the statement prematurely, so we remove them
      val withoutEmptyLines = text.split("\n").filter(_.nonEmpty).mkString("\n")
      outputStream.write((withoutEmptyLines + "\n").getBytes)
      outputStream.flush()

    repl.textSent(text)

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
