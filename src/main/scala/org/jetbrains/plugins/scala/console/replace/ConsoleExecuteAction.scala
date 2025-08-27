// This file uses modified code based on the IntelliJ Scala plugin.
// Original code can be found here:
// https://github.com/JetBrains/intellij-scala/blob/bd2ec19ced511fd2f27459ca733dde5cb432aba6/scala/scala-impl/src/org/jetbrains/plugins/scala/console/actions/ScalaConsoleExecuteAction.scala

// The reason for this class being in a separate package is that the ConsoleExecuteAction class
// uses the ScalaLanguageConsole.textSent() method, which is package private.
// Therefore, to call it, we must be in the same package as the console: org.jetbrains.plugins.scala.console.

package org.jetbrains.plugins.scala.console.replace

import com.intellij.openapi.actionSystem.{ActionUpdateThread, AnActionEvent, CommonDataKeys}
import com.intellij.openapi.application.ApplicationManager
import com.intellij.openapi.util.TextRange
import fi.aalto.cs.replace.Repl
import org.jetbrains.plugins.scala.console.ScalaConsoleInfo
import org.jetbrains.plugins.scala.console.actions.ScalaConsoleExecuteAction
import org.jetbrains.plugins.scala.extensions.inWriteAction

class ConsoleExecuteAction extends ScalaConsoleExecuteAction:

  override def actionPerformed(e: AnActionEvent): Unit =
    val editor = e.getData(CommonDataKeys.EDITOR)
    if editor == null then return // scalastyle:ignore

    val console           = ScalaConsoleInfo.getConsole(editor)
    val processHandler    = ScalaConsoleInfo.getProcessHandler(editor)
    val historyController = ScalaConsoleInfo.getController(editor)

    val document = console.getEditorDocument
    val text     = document.getText

    // We should perform our multiline fixing only for our custom REPL that is running Scala 3.
    // REPLs that host Scala 2 should not be modified.
    // Additionally, if the text has no newlines, we don't need to modify it.
    if !console.isInstanceOf[Repl]
      || !console.asInstanceOf[Repl].isScala3REPL
      || !text.exists(c => c == '\n' || c == '\r')
    then
      super.actionPerformed(e)
      return

    // Process input and add to history
    ApplicationManager.getApplication.runWriteAction(new Runnable():
      def run(): Unit =
        val range: TextRange = new TextRange(0, document.getTextLength)
        editor.getSelectionModel.setSelection(range.getStartOffset, range.getEndOffset)
        // note: it uses `range` instead ot just editor `text` because under the hood it splits actual editor content
        // according to the highlighter attributes and passes the correct ContentType to the history console
        console.addToHistory(range, console.getConsoleEditor, true)
        // without this line there will be a slight blinking of user input code SCL-16655
        // see com.intellij.execution.impl.ConsoleViewImpl.print
        console.flushDeferredText()
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

    console.textSent(text)

  override def getActionUpdateThread: ActionUpdateThread = ActionUpdateThread.BGT
