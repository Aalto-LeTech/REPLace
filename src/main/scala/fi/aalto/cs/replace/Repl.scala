package fi.aalto.cs.replace

import com.intellij.execution.process.{ProcessEvent, ProcessHandler, ProcessListener}
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
import java.io.OutputStream
import java.nio.file.Path
import scala.io.Source

class Repl(module: Module) extends ScalaLanguageConsole(module: Module):
  private val stateLock     = new Object
  private val outputTracker = new ReplOutputTracker(Repl.promptText, Repl.welcomeLine)
  private var pendingInitialCommands: List[String]                         = List.empty
  @volatile private var attachedProcessHandler: Option[ProcessHandler]     = None
  private var pendingMultilineSubmissions: List[Scala3MultilineSubmission] = List.empty
  private var multilineSubmissionsToRetry: List[Scala3MultilineSubmission] = List.empty

  private val submissionCleanupListener = new ProcessListener:
    override def processTerminated(event: ProcessEvent): Unit =
      readyForUserInput = false
      closeAllMultilineSubmissions()

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
  val isScala3REPL: Boolean                        = scala3Module
  @volatile private var readyForUserInput: Boolean = !isScala3REPL

  private[replace] def initialCommands: List[String] = getInitialReplCommands(module)
  private[replace] def scala3Module: Boolean         = ModuleUtils.isScala3Module(module)
  def isReadyForUserInput: Boolean                   = readyForUserInput

  override def attachToProcess(processHandler: ProcessHandler): Unit =
    super.attachToProcess(processHandler)
    attachedProcessHandler
      .filter(_ ne processHandler)
      .foreach(_.removeProcessListener(submissionCleanupListener))
    attachedProcessHandler = Some(processHandler)
    processHandler.addProcessListener(submissionCleanupListener)
    if processHandler.isProcessTerminated then closeAllMultilineSubmissions()

  /** Creates the temporary source file for a multiline submission. Console state is not touched, so
    * a failure here leaves the console fully usable.
    */
  def prepareMultilineSubmission(source: String): Scala3MultilineSubmission =
    Scala3MultilineSubmission.create(source)

  def sendMultilineSubmission(
      submission: Scala3MultilineSubmission,
      outputStream: OutputStream
  ): Unit =
    readyForUserInput = false
    stateLock.synchronized {
      pendingMultilineSubmissions = pendingMultilineSubmissions :+ submission
    }

    try
      submission.writeTo(outputStream)
      if attachedProcessHandler.exists(_.isProcessTerminated) then closeAllMultilineSubmissions()
    catch
      case error: Throwable =>
        stateLock.synchronized {
          pendingMultilineSubmissions = pendingMultilineSubmissions.filterNot(_ eq submission)
        }
        deleteOrRetry(submission)
        readyForUserInput = attachedProcessHandler.exists(!_.isProcessTerminated)
        throw error

  override def print(text: String, contentType: ConsoleViewContentType): Unit =
    val events = stateLock.synchronized(outputTracker.append(text))

    var updatedText = text
    if events.welcomeCompleted then
      val commands = initialCommands
      // Rewriting the banner is only possible when the console delivered the line unsplit.
      if text == Repl.welcomeLine then updatedText = getUpdatedText(module, commands, text)

      // Normally, in Scala 2, we would have used the "-i" argument to pass initial REPL commands
      // Unfortunately, this has not been ported into Scala 3
      if isScala3REPL then
        stateLock.synchronized {
          pendingInitialCommands = commands
          readyForUserInput = false
        }

    // A prompt means that the REPL is ready for another command. Sending all startup commands
    // immediately can make newer Scala 3 REPLs consume them as a single submission.
    val startupCommand = stateLock.synchronized {
      if events.promptCompleted then
        pendingInitialCommands match
          case command :: rest =>
            pendingInitialCommands = rest
            Some(command)
          case Nil =>
            readyForUserInput = true
            None
      else None
    }
    val startupInProgress =
      startupCommand.isDefined || stateLock.synchronized(pendingInitialCommands.nonEmpty)

    // A prompt also means the REPL has finished with whatever submission file it was loading.
    if events.promptCompleted then closeNextMultilineSubmission()
    startupCommand.foreach(command => ScalaExecutor.runLine(this, command))

    // In Scala 3, the REPL colors the "scala>" prompt with ANSI sequences, but the Scala plugin's
    // console state machine expects the prompt with NORMAL_OUTPUT attributes; a chunk-final
    // prompt is therefore printed separately with normalized attributes. Prompts and blank
    // spacing emitted while startup commands are still being fed are hidden.
    val (body, promptPart) =
      if events.promptCompleted && updatedText.endsWith(Repl.promptToken) then
        updatedText.splitAt(updatedText.length - Repl.promptToken.length)
      else (updatedText, "")

    if body.nonEmpty && !(startupInProgress && body.trim.isEmpty) then
      super.print(body, contentType)
    if promptPart.nonEmpty && !startupInProgress then
      super.print(promptPart, ConsoleViewContentType.NORMAL_OUTPUT)

  override def dispose(): Unit =
    readyForUserInput = false
    attachedProcessHandler.foreach(_.removeProcessListener(submissionCleanupListener))
    attachedProcessHandler = None
    closeAllMultilineSubmissions()
    changesService.foreach(_.onReplClosed(module))
    super.dispose()

  private def closeNextMultilineSubmission(): Unit =
    val submission = stateLock.synchronized {
      pendingMultilineSubmissions match
        case head :: tail =>
          pendingMultilineSubmissions = tail
          Some(head)
        case Nil => None
    }
    submission.foreach(deleteOrRetry)

  private def deleteOrRetry(submission: Scala3MultilineSubmission): Unit =
    if !submission.delete() then
      stateLock.synchronized {
        multilineSubmissionsToRetry = submission :: multilineSubmissionsToRetry
      }

  private def closeAllMultilineSubmissions(): Unit =
    val submissions = stateLock.synchronized {
      val current = pendingMultilineSubmissions ++ multilineSubmissionsToRetry
      pendingMultilineSubmissions = List.empty
      multilineSubmissionsToRetry = List.empty
      current
    }
    submissions.foreach(_.close())

object Repl:
  val initialCommandsFileName = ".repl-commands"

  private[replace] val promptText  = "scala>"
  private[replace] val promptToken = promptText + " "
  private[replace] val welcomeLine = "Type in expressions for evaluation. Or try :help.\n"

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
