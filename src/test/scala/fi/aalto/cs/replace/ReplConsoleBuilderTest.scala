package fi.aalto.cs.replace

import com.intellij.execution.ui.ConsoleView
import com.intellij.openapi.application.ApplicationManager
import com.intellij.openapi.util.Disposer
import com.intellij.testFramework.PlatformTestUtil
import org.jetbrains.plugins.scala.console.ScalaConsoleInfo
import org.junit.Assert.{assertNotSame, assertSame, assertTrue}
import org.junit.jupiter.api.Test

import java.util.concurrent.TimeUnit

class ReplConsoleBuilderTest extends ReplPlatformTestBase:

  /** The run configuration may call `createConsole()` on a pooled thread (issue #11), while the
    * console's editors may only be created on the EDT.
    */
  @Test
  def testCreatesTheConsoleFromAPooledThread(): Unit =
    val builder = new ReplConsoleBuilder(getModule)
    val future = ApplicationManager.getApplication.executeOnPooledThread[ConsoleView](() =>
      builder.createConsole()
    )

    // waitForFuture keeps dispatching the EDT's invocation events while it waits, which is what
    // the pooled thread's invokeAndWait needs to make progress at all.
    val console = PlatformTestUtil.waitForFuture(future, TimeUnit.SECONDS.toMillis(30))
    try assertTrue(console.isInstanceOf[Repl])
    finally Disposer.dispose(console)

  /** The Scala plugin registers every attached console and never removes one, and the head of that
    * per-project list answers `getConsole(project)`.
    */
  @Test
  def testDisposalUnregistersTheConsoleFromTheScalaPlugin(): Unit =
    val console = new Repl(getModule)
    withRepl(console) { (repl, _) =>
      assertSame(repl, ScalaConsoleInfo.getConsole(getProject))
    }
    assertNotSame(console, ScalaConsoleInfo.getConsole(getProject))
