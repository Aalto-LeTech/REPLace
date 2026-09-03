package fi.aalto.cs.replace

import com.intellij.ui.InplaceButton
import com.intellij.util.ui.UIUtil
import fi.aalto.cs.replace.services.PluginSettings
import fi.aalto.cs.replace.ui.ReplBannerPanel
import fi.aalto.cs.replace.utils.MyBundle.message
import org.junit.Assert.{assertFalse, assertNotNull, assertTrue}
import org.junit.jupiter.api.Test

/** The banner a course console shows when its module has been edited since the REPL started. Both
  * ways of dismissing it are user-visible promises: one lasts for the project, the other only for
  * the console it was clicked in.
  */
class ReplBannerPanelTest extends ReplPlatformTestBase:

  @Test
  def testDontShowAgainPersistsForTheWholeProject(): Unit =
    val banner = new ReplBannerPanel(getProject)
    banner.setVisible(true)

    clickLink(banner, message("ui.repl.warning.hideAlways"))

    assertFalse(banner.isVisible)
    assertTrue(
      "the choice must be remembered in the project's settings",
      PluginSettings(getProject).neverShowBanner
    )
    banner.setVisible(true)
    assertFalse("a later request to show it must be a no-op", banner.isVisible)

    val laterConsolesBanner = new ReplBannerPanel(getProject)
    laterConsolesBanner.setVisible(true)
    assertFalse(
      "later consoles of this project must stay silent too",
      laterConsolesBanner.isVisible
    )

  @Test
  def testClosingTheBannerHidesItWithoutPersistingAnything(): Unit =
    val banner = new ReplBannerPanel(getProject)
    banner.setVisible(true)

    closeButton(banner).doClick()

    assertFalse(banner.isVisible)
    assertFalse(
      "closing one banner must not silence the project",
      PluginSettings(getProject).neverShowBanner
    )
    banner.setVisible(true)
    assertFalse("the closed banner must stay closed", banner.isVisible)

    val laterConsolesBanner = new ReplBannerPanel(getProject)
    laterConsolesBanner.setVisible(true)
    assertTrue("a later console must still be able to warn", laterConsolesBanner.isVisible)

  private def clickLink(banner: ReplBannerPanel, text: String): Unit =
    val label = banner.findLabelByName(text)
    assertNotNull(s"no '$text' link on the banner", label)
    label.doClick()

  private def closeButton(banner: ReplBannerPanel): InplaceButton =
    val button = UIUtil.findComponentOfType(banner, classOf[InplaceButton])
    assertNotNull("no close button on the banner", button)
    button
