package fi.aalto.cs.replace

import com.intellij.execution.ui.ConsoleViewContentType
import com.intellij.openapi.module.Module
import com.intellij.openapi.project.Project
import fi.aalto.cs.replace.services.ReplChangesService
import fi.aalto.cs.replace.utils.ModuleUtils
import fi.aalto.cs.replace.utils.ModuleUtils.{getInitialReplCommands, getUpdatedText}
import fi.aalto.cs.replace.ui.ReplBannerPanel
import org.jetbrains.plugins.scala.console.ScalaLanguageConsole
import org.jetbrains.plugins.scala.console.replace.ScalaExecutor

import java.awt.BorderLayout
import java.awt.event.{FocusAdapter, FocusEvent}
import java.nio.file.Path
import scala.io.Source

class Repl(module: Module) extends ScalaLanguageConsole(module: Module):
  private var initialReplWelcomeMessageHasBeenReplaced: Boolean = false
  private val initialReplWelcomeMessageToBeReplaced: String =
    "Type in expressions for evaluation. Or try :help.\n"

  private val scalaPromptText: String     = "scala>"
  private var remainingPromptsToSkip: Int = 0

  private val banner = new ReplBannerPanel(module.getProject)
  banner.setVisible(false)
  add(banner, BorderLayout.NORTH)

  // Do not show the warning banner for non-A+ courses
  private val changesService: Option[ReplChangesService] =
    Option.when(Repl.isCoursesProject(module.getProject))(ReplChangesService(module.getProject))
  changesService.foreach { service =>
    // creating a new REPL resets the "module changed" state
    service.onReplStarted(module)

    // A focus listener on the console's own editors, rather than a global AWT event listener that
    // outlives the console it was installed for.
    val bannerRefresher = new FocusAdapter:
      override def focusGained(event: FocusEvent): Unit =
        banner.setVisible(service.hasModuleChanged(module))
    getConsoleEditor.getContentComponent.addFocusListener(bannerRefresher)
    getHistoryViewer.getContentComponent.addFocusListener(bannerRefresher)
  }

  // We need this here because the overridden ConsoleExecuteAction needs to determine whether
  // the console is hosting a Scala 3 REPL or something else
  val isScala3REPL: Boolean = ModuleUtils.isScala3Module(module)

  override def print(text: String, contentType: ConsoleViewContentType): Unit =
    var updatedText = text

    if text.equals(initialReplWelcomeMessageToBeReplaced)
      && !initialReplWelcomeMessageHasBeenReplaced
    then
      val commands = getInitialReplCommands(module)
      updatedText = getUpdatedText(module, commands, text)

      // Normally, in Scala 2, we would have used the "-i" argument to pass initial REPL commands
      // Unfortunately, this has not been ported into Scala 3
      if isScala3REPL then
        remainingPromptsToSkip = commands.length
        commands.foreach(cmd => ScalaExecutor.runLine(this, cmd))

      initialReplWelcomeMessageHasBeenReplaced = true

    // When auto-executing commands in Scala 3 using ScalaExecutor, the prompt is printed
    // after every execution. We hide these prompts so as not to confuse the user
    if remainingPromptsToSkip > 0 && (text.trim == scalaPromptText || text.trim == "") then
      if text.trim == scalaPromptText then remainingPromptsToSkip -= 1
    else
      // In Scala 3 REPL, the "scala>" prompt is colored blue by sending appropriate ANSI sequences
      // This is not handled correctly by Scala plugin which expects the prompt to be sent with
      // the NORMAL_OUTPUT attributes; this breaks the REPL state machine,
      // Therefore, we override the text attributes for the prompt.
      // The unintended consequence
      // of this fix is that output lines equal to the prompt will also lose their text attributes.
      super.print(
        updatedText,
        if text.trim == scalaPromptText then ConsoleViewContentType.NORMAL_OUTPUT else contentType
      )

  override def dispose(): Unit =
    changesService.foreach(_.onReplClosed(module))
    super.dispose()

object Repl:
  val initialCommandsFileName = ".repl-commands"
  def additionalArguments(project: Project): String =
    val basePath = project.getBasePath
    if basePath == null then return ""
    val file = Path.of(basePath, ".idea", ".repl-arguments").toFile
    if file.exists then
      val source = Source.fromFile(file)
      try source.mkString
      finally source.close
    else ""

  private def isCoursesProject(project: Project): Boolean =
    val basePath = project.getBasePath
    if basePath == null then return false
    val file = Path.of(basePath, ".idea", "aplus_project.xml").toFile
    file.exists
