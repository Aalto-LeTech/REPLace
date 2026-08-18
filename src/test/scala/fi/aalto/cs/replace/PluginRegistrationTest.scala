package fi.aalto.cs.replace

import com.intellij.openapi.actionSystem.ActionManager
import com.intellij.testFramework.fixtures.BasePlatformTestCase
import org.junit.Assert.{assertNotNull, assertTrue}
import org.jetbrains.plugins.scala.console.replace.ConsoleExecuteAction

class PluginRegistrationTest extends BasePlatformTestCase:

  def testOverridesScalaConsoleExecuteAction(): Unit =
    val action = ActionManager.getInstance.getAction("ScalaConsole.Execute")

    assertNotNull(action)
    assertTrue(
      s"Expected REPLace's console action, got ${action.getClass.getName}",
      action.isInstanceOf[ConsoleExecuteAction]
    )
