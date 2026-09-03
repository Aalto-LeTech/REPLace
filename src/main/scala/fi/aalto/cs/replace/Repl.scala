package fi.aalto.cs.replace

import com.intellij.execution.process.{ProcessEvent, ProcessHandler, ProcessListener}
import com.intellij.execution.ui.ConsoleViewContentType
import com.intellij.notification.{Notification, NotificationAction, NotificationType, Notifications}
import com.intellij.openapi.diagnostic.Logger
import com.intellij.openapi.module.Module
import com.intellij.openapi.progress.ProcessCanceledException
import com.intellij.openapi.project.Project
import com.intellij.openapi.util.TextRange
import com.intellij.util.concurrency.ThreadingAssertions
import com.intellij.util.concurrency.annotations.RequiresEdt
import fi.aalto.cs.replace.services.ReplChangesService
import fi.aalto.cs.replace.ui.ReplBannerPanel
import fi.aalto.cs.replace.utils.ModuleUtils.{getInitialReplCommands, welcomeText}
import fi.aalto.cs.replace.utils.{ModuleUtils, MyBundle}
import org.intellij.lang.annotations.Language
import org.jetbrains.plugins.scala.console.{ScalaConsoleInfo, ScalaLanguageConsole}
import org.jetbrains.plugins.scala.console.replace.ScalaExecutor
import org.jetbrains.plugins.scala.project.{ModuleExt, ScalaFeatures}

import java.awt.BorderLayout
import java.awt.event.{FocusAdapter, FocusEvent}
import java.io.OutputStream
import java.nio.file.{Files, Path}
import java.util.concurrent.atomic.AtomicReference
import scala.util.control.NonFatal

class Repl(val module: Module) extends ScalaLanguageConsole(module):
  private val stateLock     = new Object
  private val outputTracker = new ReplOutputTracker(Repl.promptToken, Repl.welcomeLine)

  /** The `:load` of the module's startup-commands file, until a prompt sends it. Guarded by
    * `stateLock`.
    */
  private var pendingStartupCommand: Option[String] = None

  /** True until the startup command has been armed, which the welcome line does, or the first
    * prompt for a REPL whose banner reads differently. Guarded by `stateLock`.
    */
  private var welcomePhasePending: Boolean                             = true
  @volatile private var attachedProcessHandler: Option[ProcessHandler] = None
  @volatile private var disposed: Boolean                              = false

  /** The lifecycle of one split paste. `id` matches a cancel against the paste it was posted for,
    * and `sent` turns true once the current group's `:load` line has reached the REPL, which is
    * when a prompt can acknowledge that group.
    */
  private enum Paste:
    case Idle
    case Loading(
        id: AnyRef,
        current: Scala3MultilineSubmission,
        sent: Boolean,
        rest: List[Scala3MultilineSubmission]
    )

    def submissions: List[Scala3MultilineSubmission] = this match
      case Idle                         => Nil
      case Loading(_, current, _, rest) => current :: rest

  /** The paste currently being fed to the REPL. Guarded by `stateLock`. */
  private var paste: Paste = Paste.Idle

  /** The current file of a user-abandoned paste, kept until the next prompt or process shutdown
    * because the REPL may not have opened it yet. Guarded by `stateLock`.
    */
  private var abandonedSubmission: Option[Scala3MultilineSubmission] = None

  /** True when multiline input must use the stock raw transport until the next completed prompt.
    * Only a prompt shows the REPL is back at top level, and a `:load` written into input it is
    * still reading is parsed as source text. The first completed prompt clears it, so several raw
    * submissions in quick succession can clear it before the last of their prompts arrives, and a
    * paste landing in that gap uses `:load` again. Guarded by `stateLock`.
    */
  private var rawTransportUntilPrompt: Boolean = false

  /** True only while the console action copies entered text into the history through `print`. Those
    * chunks are presentation, and must not advance the paste protocol or end the raw transport.
    * Guarded by `stateLock`.
    */
  private var printingInputHistory: Boolean = false

  /** Guarded by `stateLock`, which must be held across each classify-and-print pair. */
  private val spacingFilter = new ReplSpacingFilter(Repl.promptToken)
  private var spacingContentType: Option[ConsoleViewContentType] = None

  /** Chunk-final text that may begin the REPL's welcome line, which is replaced as a whole and so
    * must never reach the console in pieces. Guarded by `stateLock`.
    */
  private var heldWelcomeStart: String = ""

  private val submissionCleanupListener = new ProcessListener:
    override def processTerminated(event: ProcessEvent): Unit = handleProcessTermination()

  private val isCoursesProject = Repl.isCoursesProject(module.getProject)

  private val changesService: Option[ReplChangesService] =
    Option.when(isCoursesProject)(ReplChangesService(module.getProject))
  private val changeBanner: Option[ReplBannerPanel] = changesService.map { service =>
    val banner = new ReplBannerPanel(module.getProject)
    banner.setVisible(false)
    add(banner, BorderLayout.NORTH)

    service.onReplStarted(module)

    val bannerRefresher = new FocusAdapter:
      override def focusGained(event: FocusEvent): Unit =
        banner.setVisible(service.hasModuleChanged(module))
    getConsoleEditor.getContentComponent.addFocusListener(bannerRefresher)
    getHistoryViewer.getContentComponent.addFocusListener(bannerRefresher)
    banner
  }

  /** Re-checks whether the module has been edited since this REPL started. The focus listeners miss
    * an edit made without the console ever regaining focus, so the input actions refresh too.
    */
  def refreshChangeBanner(): Unit =
    for service <- changesService; banner <- changeBanner do
      banner.setVisible(service.hasModuleChanged(module))

  private[replace] def isChangeBannerVisible: Boolean = changeBanner.exists(_.isVisible)

  val isScala3REPL: Boolean                        = scala3Module
  @volatile private var readyForUserInput: Boolean = false

  /** The module's startup-commands file, or none when it has none. */
  private[replace] def initialCommandsFile: Option[Path] =
    ModuleUtils.getInitialReplCommandsFile(module)

  /** The startup-commands file and the commands written in it, empty when either cannot be read.
    * Loaded before entering `stateLock`, because finding the module directory takes an IntelliJ
    * read action and that lock order is the reverse of project disposal's. `attachToProcess` forces
    * this, so throwing here would fail the launch of an already started REPL JVM.
    */
  private lazy val initialCommandsSnapshot: (Option[Path], List[String]) =
    try
      val file = initialCommandsFile
      (file, file.fold(List.empty[String])(getInitialReplCommands))
    catch
      case cancellation: ProcessCanceledException => throw cancellation
      case NonFatal(error) =>
        Repl.logger.warn(s"Could not read the startup commands of ${module.getName}", error)
        (None, List.empty)

  /** The Scala feature set of the module's REPL, used to parse pastes the way that REPL will. */
  def parserFeatures: ScalaFeatures          = module.features
  private[replace] def scala3Module: Boolean = ModuleUtils.isScala3Module(module)
  def mustUseRawTransport: Boolean           = stateLock.synchronized { rawTransportUntilPrompt }

  /** Only a Scala 3 console paces its input against prompts. A Scala 2 prompt can be reconfigured
    * away and may never reach the tracker, so its readiness is a live process alone.
    */
  def isReadyForUserInput: Boolean =
    if isScala3REPL then readyForUserInput else processAlive

  /** Copies entered text to the history without mistaking its synthetic prompt for REPL output. */
  @RequiresEdt
  def addInputToHistory(range: TextRange): Unit =
    ThreadingAssertions.assertEventDispatchThread()
    stateLock.synchronized {
      printingInputHistory = true
      try addToHistory(range, getConsoleEditor, true)
      finally printingInputHistory = false
    }

  /** The hint posted for an Enter that could not be sent, or null. The platform retains a logged
    * balloon until it is clicked, so each new hint expires this one. The EDT, the output pump and
    * `dispose` all write it, so take-and-expire must stay one atomic `getAndSet`.
    */
  private val inputUnavailableHint = new AtomicReference[Notification]()

  /** Explains why Enter cannot be sent yet, with an escape action while a split paste runs. */
  @RequiresEdt
  def showInputUnavailableHint(): Unit =
    ThreadingAssertions.assertEventDispatchThread()
    expireInputUnavailableHint()
    val notification =
      stateLock.synchronized(paste) match
        case Paste.Loading(expectedPasteId, _, _, _) =>
          new Notification(
            Repl.notificationGroup,
            MyBundle.message("ui.repl.console.input.busy.title"),
            MyBundle.message("ui.repl.console.input.busy.message"),
            NotificationType.INFORMATION
          ).addAction(
            NotificationAction.createSimpleExpiring(
              MyBundle.message("ui.repl.console.input.busy.stop"),
              () => cancelPastePacingForUserInput(expectedPasteId)
            )
          )
        case Paste.Idle =>
          new Notification(
            Repl.notificationGroup,
            MyBundle.message("ui.repl.console.unavailable.title"),
            MyBundle.message("ui.repl.console.unavailable.message"),
            NotificationType.INFORMATION
          )
    Notifications.Bus.notify(notification, module.getProject)
    inputUnavailableHint.set(notification)
    // The output pump can restore readiness between the two, and find no hint to expire yet.
    if isReadyForUserInput then expireInputUnavailableHint()

  /** True once disposed, so work queued onto the EDT from the output pump can drop itself. */
  def isReplDisposed: Boolean = disposed

  // Called once per instance, because the run configuration builds a console per execution.
  override def attachToProcess(processHandler: ProcessHandler): Unit =
    // Resolved while the console is live, so `print` never completes it during project disposal.
    val _ = initialCommandsSnapshot
    val attached = stateLock.synchronized {
      if disposed then false
      else
        attachedProcessHandler = Some(processHandler)
        processHandler.addProcessListener(submissionCleanupListener)
        try
          super.attachToProcess(processHandler)
          true
        catch
          case error: Throwable =>
            processHandler.removeProcessListener(submissionCleanupListener)
            attachedProcessHandler = None
            throw error
    }
    // ProcessListener registration does not replay an earlier termination event.
    if attached && processHandler.isProcessTerminated then handleProcessTermination()

  /** The current process input, without depending on `ScalaConsoleInfo` registration. */
  def attachedProcessInput: Option[OutputStream] =
    attachedProcessHandler.flatMap(handler => Option(handler.getProcessInput))

  /** Recomputed on each read, because the process can terminate between any two statements. */
  private def processAlive: Boolean =
    !disposed && attachedProcessHandler.exists(!_.isProcessTerminated)

  /** Rederives readiness from the full console state. Assigned inside the lock, or it could
    * overwrite the `false` a freshly published paste just wrote.
    */
  private def restoreReadiness(): Unit = stateLock.synchronized {
    readyForUserInput = processAlive && paste == Paste.Idle && pendingStartupCommand.isEmpty
    // Safe under the lock, because expiry reaches the notification model only from the EDT, and
    // the one EDT caller that arrives with a live hint had it expired by `createSimpleExpiring`.
    if readyForUserInput then expireInputUnavailableHint()
  }

  private def expireInputUnavailableHint(): Unit =
    Option(inputUnavailableHint.getAndSet(null)).foreach(_.expire())

  /** Keeps multiline input on the raw transport until the next completed prompt. Readiness is
    * deliberately left alone, because no prompt follows an unfinished expression or a `readLine`.
    */
  def useRawTransportUntilPrompt(): Unit = stateLock.synchronized { rawTransportUntilPrompt = true }

  /** Console state is not touched, so a failure here leaves the console fully usable. */
  def prepareMultilineSubmission(
      @Language("Scala") source: String,
      lineOffset: Int
  ): Scala3MultilineSubmission =
    Scala3MultilineSubmission.create(source, lineOffset)

  /** Submits the groups of one multiline paste, the first immediately and the rest one per REPL
    * prompt, so each group compiles as its own unit. If the first write fails the whole paste is
    * cleaned up and the error propagates.
    */
  @RequiresEdt
  def sendMultilineSubmissions(
      submissions: List[Scala3MultilineSubmission],
      outputStream: OutputStream
  ): Boolean =
    ThreadingAssertions.assertEventDispatchThread()
    submissions match
      case Nil          => false
      case head :: tail =>
        // Readiness is cleared in the same critical section that publishes the paste, so no prompt
        // handler can observe one without the other.
        stateLock.synchronized {
          readyForUserInput = false
          paste = Paste.Loading(new Object, head, sent = false, tail)
        }
        try
          val sent = writeGroup(head, outputStream)
          if !sent then restoreReadiness()
          sent
        catch
          case error: Throwable =>
            takeAllSubmissions().foreach(_.close())
            restoreReadiness()
            throw error

  /** Writes one group's `:load` line and then records it as sent, which is what lets the next
    * prompt acknowledge it. The pipe write happens between the two locked sections.
    */
  private def writeGroup(submission: Scala3MultilineSubmission, stream: OutputStream): Boolean =
    val isCurrentUnsentGroup = stateLock.synchronized {
      paste match
        case Paste.Loading(_, current, false, _) => current.eq(submission)
        case _                                   => false
    }
    if !isCurrentUnsentGroup then false
    else
      submission.writeTo(stream)
      // A normal return from write/flush means the command was accepted, so the caller must not
      // restore text which was sent. A paste that ended during the write leaves nothing to mark.
      stateLock.synchronized {
        paste match
          case Paste.Loading(id, current, false, rest) if current.eq(submission) =>
            paste = Paste.Loading(id, current, sent = true, rest)
          case _ => ()
      }
      true

  /** Prints one chunk of process output. `stateLock` guards console state, and every transition and
    * the rendering that displays it stay inside it. Writes into the REPL's stdin stay outside it,
    * because blocking this pump thread under the lock while the EDT waits for it freezes the IDE.
    */
  override def print(text: String, contentType: ConsoleViewContentType): Unit =
    if disposed then return
    // Completing this takes an IntelliJ read action, which must never be taken under `stateLock`.
    val (commandsFile, commands) = initialCommandsSnapshot
    val deferred = stateLock.synchronized {
      if disposed then Repl.NoDeferredWrites
      else printAtomically(text, contentType, commandsFile, commands)
    }
    deferred.startupCommand.foreach(runStartupCommand)
    deferred.nextGroup.foreach((submission, stream) => sendNextGroup(submission, stream))

  private def printAtomically(
      text: String,
      contentType: ConsoleViewContentType,
      commandsFile: Option[Path],
      initialCommands: List[String]
  ): Repl.DeferredWrites =
    // Only stdout carries the prompt/welcome protocol. The history echo, stderr and USER_INPUT
    // bypass the tracker and the spacing filter without interrupting either.
    if printingInputHistory || contentType == ConsoleViewContentType.ERROR_OUTPUT ||
      contentType == ConsoleViewContentType.USER_INPUT
    then
      flushSpacingBeforeBypass()
      printRecorded(text, contentType)
      return Repl.NoDeferredWrites

    val chunk           = spacingFilter.normalize(text)
    val events          = outputTracker.append(chunk)
    val normalizedInput = heldWelcomeStart + chunk
    heldWelcomeStart = ""
    if normalizedInput.nonEmpty then
      flushSpacingOnOrdinaryTypeChange(contentType, events.promptCompleted)
    if events.promptCompleted then
      rawTransportUntilPrompt = false
      abandonedSubmission.foreach(_.close())
      abandonedSubmission = None

    // The welcome line can share its chunk with the lines around it, so only that one line is
    // replaced and the rest of the chunk is printed as it arrived.
    val welcomeStart =
      if events.welcomeCompleted then normalizedInput.indexOf(Repl.welcomeLine) else -1
    val rewrittenWelcome = Option.when(welcomeStart >= 0)(
      welcomeText(module, initialCommands, Repl.welcomeLine, isCoursesProject)
    )
    if welcomePhasePending && (events.welcomeCompleted || events.promptCompleted) then
      beginWelcomePhase(commandsFile)
    val displayText = rewrittenWelcome match
      case Some(welcome) => normalizedInput.patch(welcomeStart, welcome, Repl.welcomeLine.length)
      case None          => withoutWelcomeStart(normalizedInput)

    // Fed at a completed prompt, because a `:load` written into pending input is source text.
    val startupCommand = if events.promptCompleted then pendingStartupCommand else None
    if startupCommand.isDefined then pendingStartupCommand = None

    val (droppedMessage, nextGroup) =
      if events.promptCompleted then advancePasteAtPrompt() else (None, None)
    // Recomputed after the advance, so a prompt never reports readiness before its bookkeeping.
    if events.promptCompleted && startupCommand.isEmpty then restoreReadiness()

    // A hidden prompt only triggered the next group of a split paste, so the user sees one prompt
    // once the whole paste has finished rather than one per group.
    val rendered = spacingFilter.render(
      displayText,
      promptCompleted = events.promptCompleted,
      promptHidden = nextGroup.isDefined,
      suppressBlank = startupCommand.isDefined
    )
    if rendered.body.nonEmpty then printBody(rendered.body, rewrittenWelcome, contentType)
    // The failure notice lands above the prompt, so the console still ends at a usable prompt.
    droppedMessage.foreach(printRecorded(_, ConsoleViewContentType.ERROR_OUTPUT))
    if rendered.prompt.nonEmpty then
      printRecorded(rendered.prompt, ConsoleViewContentType.NORMAL_OUTPUT)
    Repl.DeferredWrites(startupCommand, nextGroup)

  /** Feeds the startup command to the REPL, once `print` has released `stateLock`. The command is
    * already popped, so a failure only has to restore readiness rather than strand the pump.
    */
  private def runStartupCommand(command: String): Unit =
    try ScalaExecutor.runLine(this, command)
    catch
      case NonFatal(error) =>
        Repl.logger.warn(s"Could not run startup command '$command'", error)
        restoreReadiness()

  /** Writes the group a completed prompt released, once `print` has released `stateLock`. A write
    * that throws or finds the paste gone ends the paste, or no later prompt could advance it.
    */
  private def sendNextGroup(submission: Scala3MultilineSubmission, stream: OutputStream): Unit =
    val sent =
      try writeGroup(submission, stream)
      catch
        case NonFatal(error) if !error.isInstanceOf[ProcessCanceledException] =>
          Repl.logger.warn("Could not submit the next group of a multiline paste", error)
          false
        // Cancellation is NonFatal too, so the guard above leaves it to this branch.
        case error: Throwable =>
          abortPasteAfterUnsentGroup()
          throw error
    if !sent then abortPasteAfterUnsentGroup()

  /** Ends a paste whose next group never reached the REPL. The prompt that released it was hidden,
    * so a live console is left at a prompt of its own and the undelivered groups are reported.
    */
  private def abortPasteAfterUnsentGroup(): Unit = stateLock.synchronized {
    val dropped = takeAllSubmissions()
    dropped.foreach(_.close())
    restoreReadiness()
    // One critical section, so no other chunk lands between restoring readiness and this notice.
    if !disposed && processAlive then
      if dropped.nonEmpty then
        printRecorded(Repl.droppedMessage(dropped.size), ConsoleViewContentType.ERROR_OUTPUT)
      printRecorded(Repl.promptToken, ConsoleViewContentType.NORMAL_OUTPUT)
  }

  /** Arms the startup command on the chunk that completes the REPL's welcome line, or on the first
    * prompt when that line never arrives. The whole file is loaded as one command, because dotty
    * answers an incomplete line with a continuation prompt that a dumb terminal never prints, and
    * because it compiles a loaded file as one unit the way Scala 2's `-i` does.
    */
  private def beginWelcomePhase(commandsFile: Option[Path]): Unit =
    stateLock.synchronized {
      welcomePhasePending = false
      pendingStartupCommand =
        if isScala3REPL then commandsFile.map(file => s":load $file") else None
      readyForUserInput = false
    }

  /** The text minus its chunk-final partial line, held for the next chunk to reclaim so the welcome
    * line is never printed in pieces. This cut runs before `spacingFilter.render`, so the filter's
    * held tail always precedes `heldWelcomeStart` in the output stream, which is the order
    * `normalizedInput` and `flushSpacingBeforeBypass` must concatenate them in.
    */
  private def withoutWelcomeStart(text: String): String =
    if !outputTracker.welcomePending then text
    else
      heldWelcomeStart = text.drop(text.lastIndexOf('\n') + 1)
      text.dropRight(heldWelcomeStart.length)

  /** Must be called with `stateLock` held, keeping the filter's end-of-line state in step. */
  private def printRecorded(text: String, contentType: ConsoleViewContentType): Unit =
    super.print(text, contentType)
    spacingFilter.recordPrinted(text)

  /** Prints one rendered body, with the rewritten welcome inside it split into styled segments.
    * `ScalaLanguageConsole` coerces its two-argument `print` to one content type while it prints a
    * welcome, so the segments go through `ConsoleViewImpl`'s three-argument `print` instead. The
    * text around them keeps the two-argument one, which is also what leaves the console in its
    * welcome state, so the prompt that follows is not spaced off as the end of system output. Must
    * be called with `stateLock` held.
    */
  private def printBody(
      body: String,
      welcome: Option[String],
      contentType: ConsoleViewContentType
  ): Unit =
    welcome.map(text => (text, body.indexOf(text))) match
      case Some((welcomeText, start)) if start >= 0 =>
        val before = body.take(start)
        val after  = body.drop(start + welcomeText.length)
        if before.nonEmpty then printRecorded(before, contentType)
        WelcomeStyling.welcomeSegments(welcomeText, module.getName).foreach { (segment, style) =>
          print(segment, WelcomeStyling.contentTypeFor(style), null)
        }
        spacingFilter.recordPrinted(welcomeText)
        if after.nonEmpty then printRecorded(after, contentType)
      // No shape of the welcome lets the filter hold part of it back, but a body without it in
      // full is still printable as it is.
      case _ => printRecorded(body, contentType)

  /** Flushes stdout withheld ahead of stderr or user input, which bypass prompt filtering. */
  private def flushSpacingBeforeBypass(keepCarriageReturn: Boolean = false): Unit =
    val leftover = spacingFilter.flush(heldWelcomeStart, keepCarriageReturn)
    heldWelcomeStart = ""
    if leftover.nonEmpty then
      printRecorded(leftover, spacingContentType.getOrElse(ConsoleViewContentType.NORMAL_OUTPUT))

  /** Preserves a held tail's styling when an unrelated output run follows. A chunk that completes a
    * prompt is exempt, because its spacing and the prompt must stay joined.
    */
  private def flushSpacingOnOrdinaryTypeChange(
      next: ConsoleViewContentType,
      promptCompleted: Boolean
  ): Unit =
    spacingContentType.foreach { previous =>
      // The chunk is normalized by now, so a carriage return it ends with stays held; releasing it
      // here would put it ahead of the very text it follows.
      if previous != next && !promptCompleted then
        flushSpacingBeforeBypass(keepCarriageReturn = true)
    }
    spacingContentType = Some(next)

  /** Ends the paste protocol and returns every submission file for the caller to delete. Must be
    * called with `stateLock` held, or `print` could overwrite this `false` with a stale `true`.
    */
  private def takePasteState(): List[Scala3MultilineSubmission] =
    readyForUserInput = false
    rawTransportUntilPrompt = false
    val all = paste.submissions ++ abandonedSubmission
    paste = Paste.Idle
    abandonedSubmission = None
    all

  private def handleProcessTermination(): Unit =
    val submissions = stateLock.synchronized {
      val all = takePasteState()
      // No further chunk will ever reclaim text the filter still holds back.
      if !disposed then flushSpacingBeforeBypass()
      all
    }
    submissions.foreach(_.close())

  /** Escape hatch for a split paste whose current group is blocked, for example in `readLine`. It
    * sends nothing and leaves the editor intact, so the next Enter takes the normal path.
    */
  @RequiresEdt
  private def cancelPastePacingForUserInput(expectedPasteId: AnyRef): Unit =
    ThreadingAssertions.assertEventDispatchThread()
    val queuedToClose = stateLock.synchronized {
      paste match
        case Paste.Loading(currentPasteId, current, _, queued)
            if currentPasteId.eq(expectedPasteId) =>
          paste = Paste.Idle
          abandonedSubmission = Some(current)
          rawTransportUntilPrompt = true
          Some(queued)
        case _ => None
    }
    queuedToClose.foreach { queued =>
      queued.foreach(_.close())
      restoreReadiness()
      print(Repl.inputResetMessage, ConsoleViewContentType.ERROR_OUTPUT)
    }

  override def dispose(): Unit =
    val submissions = stateLock.synchronized {
      disposed = true
      takePasteState()
    }
    attachedProcessHandler.foreach(_.removeProcessListener(submissionCleanupListener))
    attachedProcessHandler = None
    expireInputUnavailableHint()
    submissions.foreach(_.close())
    changesService.foreach(_.onReplClosed(module))
    // The Scala plugin's registry has no cleanup of its own, and its head per project is what
    // getConsole(project) answers with.
    ScalaConsoleInfo.disposeConsole(this)
    super.dispose()

  /** Acknowledges the group the REPL just finished loading and picks the next one in one atomic
    * transition, so a group is never both awaiting its prompt and queued. Only a group recorded as
    * sent has a completion prompt to acknowledge, because advancing on any earlier prompt would
    * delete that group unsent. Must be called with `stateLock` held.
    */
  private def advancePasteAtPrompt()
      : (Option[String], Option[(Scala3MultilineSubmission, OutputStream)]) =
    val nextStream = if processAlive then attachedProcessInput else None
    paste match
      case Paste.Loading(id, current, true, head :: tail) =>
        current.close()
        nextStream match
          case Some(stream) =>
            paste = Paste.Loading(id, head, sent = false, tail)
            (None, Some((head, stream)))
          case None =>
            paste = Paste.Idle
            val dropped = head :: tail
            dropped.foreach(_.close())
            restoreReadiness()
            (Some(Repl.droppedMessage(dropped.size)), None)
      case Paste.Loading(_, current, true, Nil) =>
        paste = Paste.Idle
        current.close()
        (None, None)
      case _ => (None, None)

  private def takeAllSubmissions(): List[Scala3MultilineSubmission] = stateLock.synchronized {
    val all = paste.submissions
    paste = Paste.Idle
    all
  }

object Repl:
  private val logger = Logger.getInstance(classOf[Repl])

  /** The pipe writes one `print` decided on and must perform after releasing `stateLock`. */
  private case class DeferredWrites(
      startupCommand: Option[String],
      nextGroup: Option[(Scala3MultilineSubmission, OutputStream)]
  )

  private val NoDeferredWrites = DeferredWrites(None, None)

  private def droppedMessage(dropped: Int): String =
    "\n" + MyBundle.message("ui.repl.console.input.dropped", dropped) + "\n"

  private val inputResetMessage: String =
    "\n" + MyBundle.message("ui.repl.console.input.reset") + "\n"

  /** The notification group declared in plugin.xml. */
  private[replace] val notificationGroup = "REPLace"

  private[replace] val promptToken = "scala> "
  private[replace] val welcomeLine = "Type in expressions for evaluation. Or try :help.\n"

  /** The named file inside the project's `.idea` directory, when it exists. */
  private def ideaFile(project: Project, name: String): Option[Path] =
    Option(project.getBasePath).map(Path.of(_, ".idea", name)).filter(Files.exists(_))

  def additionalArguments(project: Project): String =
    ideaFile(project, ".repl-arguments").fold("")(Files.readString(_).trim)

  private def isCoursesProject(project: Project): Boolean =
    ideaFile(project, "aplus_project.xml").isDefined
