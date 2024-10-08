// The reason for this class being in a separate package is that the runLine method
// uses the ScalaLanguageConsole.textSent() method, which is package private.
// Therefore, to call it, we must be in the same package as the console: org.jetbrains.plugins.scala.console.

package org.jetbrains.plugins.scala.console.replace

import com.intellij.openapi.application.ApplicationManager
import fi.aalto.cs.replace.Repl

import org.jetbrains.plugins.scala.console.ScalaConsoleInfo

object ScalaExecutor:
  /** Runs a single line of Scala code in the context of the provided REPL console.
    * @param console
    *   An instance of our A+ enhanced REPL.
    * @param command
    *   A single line (no newlines) of Scala code to execute.
    */
  def runLine(console: Repl, command: String): Unit =
    val processHandler = ScalaConsoleInfo.getProcessHandler(console.getConsoleEditor)
    if processHandler == null then return

    val outputStream = processHandler.getProcessInput
    if outputStream != null then
      outputStream.write((command + "\n").getBytes)
      outputStream.flush()

    // this must be invoked from EDT because it accesses the IntelliJ PSI
    ApplicationManager.getApplication.invokeLater(() => console.textSent(command))
