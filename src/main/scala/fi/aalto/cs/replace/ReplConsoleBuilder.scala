package fi.aalto.cs.replace

import com.intellij.execution.filters.TextConsoleBuilderImpl
import com.intellij.execution.ui.ConsoleView
import com.intellij.openapi.module.Module
import org.jetbrains.plugins.scala.extensions.invokeAndWait

/** Builds the REPLace console for a run configuration.
  *
  * The run configuration may call [[createConsole]] on a pooled thread, but the console's editors
  * and documents may only be created on the EDT (issue #11), so construction is dispatched there.
  */
private[replace] final class ReplConsoleBuilder(module: Module)
    extends TextConsoleBuilderImpl(module.getProject):

  override def createConsole(): ConsoleView = invokeAndWait(new Repl(module))
