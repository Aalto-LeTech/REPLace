package fi.aalto.cs.replace

import com.intellij.notification.NotificationGroupManager
import com.intellij.openapi.actionSystem.ActionManager
import com.intellij.testFramework.junit5.TestApplication
import fi.aalto.cs.replace.actions.{ReplAction, SendSelectionToConsoleAction}
import org.junit.Assert.{assertNotNull, assertTrue}
import org.jetbrains.plugins.scala.console.replace.ConsoleExecuteAction
import org.junit.jupiter.api.Test

@TestApplication
class PluginRegistrationTest:

  /** The Scala plugin's action must have been replaced by REPLace's own, not merely registered. */
  private def assertActionIsProvidedBy(actionId: String, expected: Class[?]): Unit =
    val action = ActionManager.getInstance.getAction(actionId)

    assertNotNull(s"$actionId is not registered", action)
    assertTrue(
      s"Expected ${expected.getName} for $actionId, got ${action.getClass.getName}",
      expected.isInstance(action)
    )

  @Test
  def testOverridesScalaConsoleExecuteAction(): Unit =
    assertActionIsProvidedBy("ScalaConsole.Execute", classOf[ConsoleExecuteAction])

  @Test
  def testOverridesTheConsoleLaunchAction(): Unit =
    assertActionIsProvidedBy("Scala.RunConsole", classOf[ReplAction])

  @Test
  def testOverridesTheSendSelectionToConsoleAction(): Unit =
    assertActionIsProvidedBy("Scala.SendSelectionToConsole", classOf[SendSelectionToConsoleAction])

  @Test
  def testRegistersTheNotificationGroupEveryNotificationUses(): Unit =
    // Every Notification this plugin posts names this group id; an unregistered group would leave
    // them all undisplayed.
    assertNotNull(
      "the REPLace notification group is not registered",
      NotificationGroupManager.getInstance.getNotificationGroup("REPLace")
    )
