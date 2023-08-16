package fi.aalto.cs.replace.intellij

import com.intellij.execution.process.ProcessHandler
import com.intellij.execution.ui.{ConsoleViewContentType, ObservableConsoleView}
import com.intellij.openapi.Disposable
import com.intellij.openapi.module.{Module, ModuleUtilCore}
import fi.aalto.cs.replace.intellij.utils.ModuleUtils.getAndHidePropmt
//import fi.aalto.cs.replace.intellij.services.PluginSettings
import fi.aalto.cs.replace.intellij.utils.ModuleUtils.{getInitialReplCommands, getUpdatedText}
import fi.aalto.cs.replace.intellij.utils.{ModuleUtils, ReplChangesObserver}
//import fi.aalto.cs.replace.ui.ReplBannerPanel
import org.jetbrains.plugins.scala.console.ScalaLanguageConsole
import org.jetbrains.plugins.scala.console.replace.ScalaExecutor
import org.jetbrains.plugins.scala.console.ScalaConsoleInfo
import scala.collection.mutable

import java.awt.AWTEvent
import java.awt.BorderLayout
import java.awt.Component
import java.awt.Toolkit
import javax.swing.SwingUtilities
import scala.quoted.*

class Repl(module: Module) extends ScalaLanguageConsole(module: Module):

  private var initialReplWelcomeMessageHasBeenReplaced: Boolean = false
  private val initialReplWelcomeMessageToBeReplaced: String =
    "Type in expressions for evaluation. Or try :help.\n"

  private val scalaPromptText: String = "scala>"
  private var remainingPromptsToSkip: Int = 0
  private var codeExplanationsOn: Boolean = true


//  private val banner = new ReplBannerPanel()
//  banner.setVisible(false)
//  add(banner, BorderLayout.NORTH)

  // Do not show the warning banner for non-A+ courses
//  if (PluginSettings.getInstance.getCourseProject(module.getProject) != null) {
    // creating a new REPL resets the "module changed" state
//  ReplChangesObserver.onStartedRepl(module)

//  Toolkit.getDefaultToolkit.addAWTEventListener((event: AWTEvent) => {
//    if (SwingUtilities.isDescendingFrom(event.getSource.asInstanceOf[Component], this)) {
//      banner.setVisible(ReplChangesObserver.hasModuleChanged(module))
//    }
//  }, AWTEvent.FOCUS_EVENT_MASK)
//  }

  // We need this here because the overridden ConsoleExecuteAction needs to determine whether
  // the console is hosting a Scala 3 REPL or something else
  val isScala3REPL: Boolean = ModuleUtils.isScala3Module(module)
  private val isO1Library: Boolean = module.getName == "O1Library"

  private var pastHistory = ""
  private var readingOutput = false
  private var latestOutput = ""
  private var inputBuffer = Array[String]()
  private var outputBuffer = Array[String]()
  private var userInput = ""
  private var output = ""
  private var inspectingLines: Boolean = false
  private var inspectPromptsToSkip: Int = 0

  private def isError: Boolean =
    val pattern = """^\d+\s(erro(r|rs) found)""".r
    val patternMatches: Boolean = pattern matches(output)
    patternMatches| output.contains("longer explanation available when compiling with `-explain`")

  private def shouldInspectCode(): Boolean =
    isO1Library && isScala3REPL
      && userInput.nonEmpty && output.nonEmpty
      && !userInput.contains("TreeParser.inspectLine")
      && !userInput.contains("import")
      && !isError

  override def print(text: String, contentType: ConsoleViewContentType): Unit =
    val latestCommand = getHistory.substring(pastHistory.length)
    pastHistory = getHistory
    if isO1Library then
      if latestCommand.nonEmpty then
        output = ""
        userInput = ""
        inputBuffer = Array[String]()
        outputBuffer = Array[String]()
        println(Console.YELLOW + "Latest command: " + latestCommand)
        inputBuffer = latestCommand.trim.split("\n")

        if inputBuffer.length == 1 then
          if !latestCommand.contains("TreeParser.inspectLine") then
            readingOutput = true
            userInput = inputBuffer.head

      if readingOutput then
        if text.trim == scalaPromptText then
          readingOutput = false
          outputBuffer = latestOutput.trim.split("\n")
          if outputBuffer.length > 0 then
            output = outputBuffer.last
            println(Console.YELLOW + "Latest output: " + output)
          latestOutput = ""
        else
          latestOutput += text

      if shouldInspectCode() then
        println(Console.MAGENTA + "RAN INSPECTLINE, output: " + output)
        println(Console.MAGENTA + "input: " + userInput)
        ScalaExecutor.runLine(this, s"""TreeParser.inspectLine({${userInput}},\"\"\"${userInput}\"\"\", \"\"\"${output}\"\"\")""")
        inspectingLines = true
        inspectPromptsToSkip = inputBuffer.length

        inputBuffer = Array[String]()
        outputBuffer = Array[String]()
        userInput = ""
        output = ""

    var updatedText = text

    if inspectingLines && (text.trim() == scalaPromptText) && inspectPromptsToSkip > 0 then
      updatedText = getAndHidePropmt(text)
      inspectPromptsToSkip -= 1
      if inspectPromptsToSkip == 0 then
        inspectingLines = false



    if text.equals(initialReplWelcomeMessageToBeReplaced)
      && !initialReplWelcomeMessageHasBeenReplaced then

      val commands = if module.getName == "O1Library" then
        getInitialReplCommands(module)  :+ s"import o1.util.TreeParser"
      else
        getInitialReplCommands(module)

      updatedText = getUpdatedText(module, commands, text)

      // Normally, in Scala 2, we would have used the "-i" argument to pass initial REPL commands
      // Unfortunately, this has not been ported into Scala 3
      if isScala3REPL then
        remainingPromptsToSkip = commands.length
        commands.foreach(cmd =>
          ScalaExecutor.runLine(this, cmd))
        // ScalaExecutor.runLine(this, s"TreeParser.inspect({val x = 1 + 2 + 3\nval y = 1 * 2 * 3})")
      initialReplWelcomeMessageHasBeenReplaced = true

    // When auto-executing commands in Scala 3 using ScalaExecutor, the prompt is printed
    // after every execution. We hide these prompts so as not to confuse the user
    if remainingPromptsToSkip > 0 && (text.trim == scalaPromptText || text.trim == "") then
      if text.trim == scalaPromptText then
        remainingPromptsToSkip -= 1
      end if
    else
      // In Scala 3 REPL, the "scala>" prompt is colored blue by sending appropriate ANSI sequences
      // This is not handled correctly by Scala plugin which expects the prompt to be sent with
      // the NORMAL_OUTPUT attributes; this breaks the REPL state machine
      // Therefore we override the text attributes for the prompt. The unintended consequence
      // of this fix is that output lines equal to the prompt will also lose their text attributes.
      super.print(updatedText,
        if text.trim == scalaPromptText then ConsoleViewContentType.NORMAL_OUTPUT else contentType
      )

    end if

