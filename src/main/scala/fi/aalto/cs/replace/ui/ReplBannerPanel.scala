package fi.aalto.cs.replace.ui

import com.intellij.openapi.actionSystem.IdeActions
import com.intellij.openapi.project.Project
import com.intellij.ui.EditorNotificationPanel
import fi.aalto.cs.replace.services.PluginSettings
import fi.aalto.cs.replace.utils.MyBundle.message

private[replace] final class ReplBannerPanel(project: Project)
    extends EditorNotificationPanel(EditorNotificationPanel.Status.Warning):
  private var isHiddenForSession = false

  text(message("ui.repl.warning.description"))
  createActionLabel(message("ui.repl.warning.restart"), IdeActions.ACTION_RERUN)
  createActionLabel(
    message("ui.repl.warning.hideAlways"),
    () =>
      PluginSettings(project).neverShowBanner = true
      setVisible(false)
  )
  setCloseAction(() =>
    isHiddenForSession = true
    setVisible(false)
  )

  override def setVisible(isVisible: Boolean): Unit =
    val neverShow = PluginSettings(project).neverShowBanner
    super.setVisible(isVisible && !isHiddenForSession && !neverShow)
