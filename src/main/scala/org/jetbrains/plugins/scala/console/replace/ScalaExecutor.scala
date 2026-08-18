// The reason for this class being in a separate package is that the runLine method
// uses the ScalaLanguageConsole.textSent() method, which is package private.
// Therefore, to call it, we must be in the same package as the console: org.jetbrains.plugins.scala.console.

package org.jetbrains.plugins.scala.console.replace

import fi.aalto.cs.replace.Repl

import org.jetbrains.plugins.scala.console.ScalaConsoleInfo
import org.jetbrains.plugins.scala.extensions.invokeAndWait

import java.nio.charset.StandardCharsets.UTF_8

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
      outputStream.write((command + "\n").getBytes(UTF_8))
      outputStream.flush()

    // This must finish on EDT before another prompt can trigger the next startup command.
    invokeAndWait { console.textSent(command) }
