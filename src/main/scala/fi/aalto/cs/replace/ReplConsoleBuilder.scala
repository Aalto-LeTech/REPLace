package fi.aalto.cs.replace

import com.intellij.execution.filters.TextConsoleBuilderImpl
import com.intellij.execution.ui.ConsoleView
import com.intellij.openapi.module.Module
import com.intellij.util.concurrency.annotations.RequiresReadLockAbsence
import org.jetbrains.plugins.scala.console.ScalaConsoleInfo
import org.jetbrains.plugins.scala.extensions.invokeAndWait

/** Builds the REPLace console for a run configuration.
  *
  * The run configuration may call [[createConsole]] on a pooled thread, but the console's editors
  * and documents may only be created on the EDT (issue #11), so construction is dispatched there.
  */
private[replace] final class ReplConsoleBuilder(module: Module)
    extends TextConsoleBuilderImpl(module.getProject):

  /** Blocks until the EDT has built the console, so a caller holding a read lock would deadlock
    * against an EDT waiting for the write lock.
    */
  @RequiresReadLockAbsence
  override def createConsole(): ConsoleView = invokeAndWait {
    val console = new Repl(module)
    // The stock Scala console builder sets this flag too; inspections and the import optimizer
    // read it to leave the console's input file alone.
    ScalaConsoleInfo.setIsConsole(console.getFile, true)
    console
  }
