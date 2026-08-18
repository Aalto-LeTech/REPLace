package fi.aalto.cs.replace

import com.intellij.execution.ui.ConsoleView
import com.intellij.openapi.application.ApplicationManager
import com.intellij.openapi.util.Disposer
import com.intellij.testFramework.fixtures.BasePlatformTestCase
import com.intellij.util.ui.UIUtil
import org.junit.Assert.assertTrue

import java.util.concurrent.TimeUnit

class ReplConsoleBuilderTest extends BasePlatformTestCase:

  /** The run configuration may call `createConsole()` on a pooled thread (issue #11), while the
    * console's editors may only be created on the EDT.
    */
  def testCreatesTheConsoleFromAPooledThread(): Unit =
    val builder = new ReplConsoleBuilder(getModule)
    val future = ApplicationManager.getApplication.executeOnPooledThread[ConsoleView](() =>
      builder.createConsole()
    )

    val deadline = System.nanoTime() + TimeUnit.SECONDS.toNanos(30)
    while !future.isDone && System.nanoTime() < deadline do
      UIUtil.dispatchAllInvocationEvents()
      Thread.`yield`()
    assertTrue("console creation timed out", future.isDone)

    val console = future.get()
    try assertTrue(console.isInstanceOf[Repl])
    finally Disposer.dispose(console)
