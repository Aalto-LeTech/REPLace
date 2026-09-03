// runLine calls ScalaLanguageConsole.textSent(), which is private[console] and therefore visible
// to this subpackage of org.jetbrains.plugins.scala.console.

package org.jetbrains.plugins.scala.console.replace

import fi.aalto.cs.replace.Repl

import com.intellij.openapi.application.{ApplicationManager, ModalityState}
import com.intellij.openapi.util.Condition
import com.intellij.util.concurrency.annotations.{RequiresBackgroundThread, RequiresReadLockAbsence}

import java.io.IOException
import java.nio.charset.StandardCharsets.UTF_8

object ScalaExecutor:
  /** Runs one line of Scala code (no newlines) in the given REPL console. The caller must hold no
    * read lock, because this blocks in the pipe write while the EDT may await the write lock.
    */
  @RequiresBackgroundThread(generateAssertion = false)
  @RequiresReadLockAbsence
  def runLine(console: Repl, command: String): Unit =
    val outputStream = console.attachedProcessInput.getOrElse {
      throw new IOException("The Scala REPL process has no input stream")
    }
    outputStream.write((command + "\n").getBytes(UTF_8))
    outputStream.flush()
    // invokeLater, or an EDT already waiting on this console would deadlock, and one command per
    // prompt with a FIFO EDT queue keeps the echo ordered. `textSent` touches PSI, so no `any()`
    // modality, and the expiry condition keeps it off a disposed console.
    ApplicationManager.getApplication.invokeLater(
      () => console.textSent(command + "\n"),
      ModalityState.nonModal(),
      ((_: Any) => console.isReplDisposed): Condition[Any]
    )
