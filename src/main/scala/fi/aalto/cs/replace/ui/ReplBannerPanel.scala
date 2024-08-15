package fi.aalto.cs.replace.ui

import com.intellij.openapi.project.Project
import com.intellij.ui.JBColor
import com.intellij.ui.components.ActionLink
import com.intellij.ui.components.panels.OpaquePanel
import com.intellij.util.ui.JBUI
import fi.aalto.cs.replace.services.PluginSettings
import fi.aalto.cs.replace.utils.MyBundle.message

import java.awt.event.ActionEvent
import java.awt.{BorderLayout, Color, Dimension, FlowLayout}
import javax.swing.{BorderFactory, JLabel, JPanel}

class ReplBannerPanel(private val project: Project) extends JPanel(BorderLayout()):
  private var isPermanentlyHidden = false

  private val containerPanel: JPanel = OpaquePanel(FlowLayout(FlowLayout.LEFT))

  private val infoText          = JLabel(message("ui.repl.warning.description"))
  private val dontShowOnceText  = JLabel(message("ui.repl.warning.ignoreOnce"))
  private val neverAskAgainText = JLabel(message("ui.repl.warning.ignoreAlways"))
  private val dontShowOnce = ActionLink(
    message("ui.repl.warning.ignoreOnce"),
    (_: ActionEvent) =>
      isPermanentlyHidden = true
      setVisible(false)
  )
  private val neverAskAgain = ActionLink(
    message("ui.repl.warning.ignoreAlways"),
    (_: ActionEvent) =>
      PluginSettings(project).neverShowBanner = true
      setVisible(false)
  )

  containerPanel.setBorder(JBUI.Borders.empty(5, 0, 5, 5))
  containerPanel.setMinimumSize(Dimension(0, 0))
  containerPanel.add(infoText)
  containerPanel.add(JLabel("|"))
  containerPanel.add(dontShowOnce)
  containerPanel.add(JLabel("|"))
  containerPanel.add(neverAskAgain)

  add(containerPanel)

  setBorder(
    BorderFactory.createCompoundBorder(
      BorderFactory.createMatteBorder(0, 0, 1, 0, JBColor.border()),
      BorderFactory.createEmptyBorder(0, 5, 0, 5)
    )
  )

  override def setVisible(isVisible: Boolean): Unit =
    if isVisible then
      val bgColor = JBColor(Color(200, 0, 0), Color(100, 0, 0))
      containerPanel.setBackground(bgColor)
      setBackground(bgColor)
    end if
    val neverShow = PluginSettings(project).neverShowBanner
    super.setVisible(isVisible && !isPermanentlyHidden && !neverShow)
  end setVisible
