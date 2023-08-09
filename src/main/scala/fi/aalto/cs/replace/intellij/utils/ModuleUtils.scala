package fi.aalto.cs.replace.intellij.utils

import com.intellij.openapi.actionSystem.{CommonDataKeys, DataContext}
import com.intellij.openapi.fileEditor.FileDocumentManager
import com.intellij.openapi.keymap.KeymapManager
import com.intellij.openapi.module.{Module, ModuleUtilCore}
import com.intellij.openapi.project.Project
import com.intellij.openapi.roots.{ModuleRootManager, OrderEnumerator}
import com.intellij.openapi.util.io.FileUtilRt
import fi.aalto.cs.replace.intellij.services.PluginSettings
import fi.aalto.cs.replace.utils.PluginResourceBundle.{getAndReplaceText, getText}
import org.apache.commons.io.FileUtils
import org.jetbrains.annotations.NotNull
import org.slf4j.LoggerFactory

import java.io.IOException
import java.nio.charset.StandardCharsets
import java.nio.file.Paths
import scala.jdk.javaapi.CollectionConverters.asJava

object ModuleUtils:

  private val Logger = LoggerFactory.getLogger(ModuleUtils.getClass)

  def getModuleDirectory(@NotNull module: Module): String =
    FileUtilRt.toSystemIndependentName(ModuleUtilCore.getModuleDirPath(module))

  def getModuleOfEditorFile(@NotNull project: Project,
                            @NotNull dataContext: DataContext): Option[Module] =
    Option(CommonDataKeys.EDITOR.getData(dataContext))
      .flatMap(editor => Option(FileDocumentManager.getInstance.getFile(editor.getDocument)))
      .flatMap(openFile => Option(ModuleUtilCore.findModuleForFile(openFile, project)))

  def getModuleOfSelectedFile(@NotNull project: Project,
                              @NotNull dataContext: DataContext): Option[Module] =
    Option(CommonDataKeys.VIRTUAL_FILE.getData(dataContext))
      .flatMap(file => Option(ModuleUtilCore.findModuleForFile(file, project)))

  def nonEmpty(enumerator: OrderEnumerator): Boolean =
    var nonEmpty = false
    enumerator.forEach { _ =>
      nonEmpty = true
      false
    }
    nonEmpty

  // O1_SPECIFIC
  def naiveValidate(@NotNull command: String): Boolean =
    command.matches("import\\so1\\.[a-z]*(\\*|\\.\\*)$")

  def clearCommands(@NotNull imports: List[String]): List[String] =
    imports
      .map(_.replace("import ", ""))
      .map(_.replace(".*", ""))

  def getCommandsText(@NotNull imports: List[String]): String =
    imports.length match
      case 0 => ""
      case 1 => getAndReplaceText("ui.repl.console.welcome.autoImport.single.message", imports.head)
      case _ => getAndReplaceText("ui.repl.console.welcome.autoImport.multiple.message", imports.mkString(", "))

  def getAndHidePropmt(@NotNull originalText: String): String =

      getAndReplaceText("ui,repl.console.scala.repl.replacePrompt", originalText)


  def getUpdatedText(@NotNull module: Module,
                     @NotNull commands: List[String],
                     @NotNull originalText: String): String =
    val runConsoleShortCut = getPrettyKeyMapString("Scala.RunConsole")
    val executeConsoleShortCut = getPrettyKeyMapString("ScalaConsole.Execute")
    val reRunShortCut = getPrettyKeyMapString("Rerun")
    val editorUpShortCut = getPrettyKeyMapString("EditorUp")
    val editorDownShortCut = getPrettyKeyMapString("EditorDown")

    val commonText = getAndReplaceText("ui.repl.console.welcome.commonText",
      executeConsoleShortCut, editorUpShortCut, editorDownShortCut, reRunShortCut) + "\n"

    if isTopLevelModule(module) then
      getAndReplaceText("ui.repl.console.welcome.noModuleText",
        commonText, originalText, runConsoleShortCut)
    else
      val validCommands = commands.filter(command => naiveValidate(command))
      val clearedCommands = clearCommands(validCommands)
      val commandsText = getCommandsText(clearedCommands)

      getAndReplaceText("ui.repl.console.welcome.fullText",
        module.getName, commandsText, commonText, originalText)

  @NotNull
  def getPrettyKeyMapString(@NotNull actionId: String): String =
    val shortCuts = KeymapManager
      .getInstance
      .getActiveKeymap
      .getShortcuts(actionId)

    if shortCuts.nonEmpty then
      shortCuts
        .head
        .toString
        .replace("[", "")
        .replace("]", "")
        .split(" ")
        .filter(_ != "pressed")
        .map(_.toLowerCase)
        .map(_.capitalize)
        .mkString("+")
    else
      "ui.repl.console.welcome.shortcutMissing"

  def getModuleRoot(@NotNull moduleFilePath: String): String =
    val lastIndexOf = moduleFilePath.lastIndexOf("/")
    moduleFilePath.substring(0, lastIndexOf + 1) // scalastyle:ignore

  /**
   * Creates the initial REPL commands file if it does not exist yet, otherwise does nothing.
   */
  def createInitialReplCommandsFile(@NotNull module: Module): Unit =
    val commands = getInitialReplCommands(module)
    val file = Paths
      .get(getModuleDirectory(module), PluginSettings.MODULE_REPL_INITIAL_COMMANDS_FILE_NAME)
      .toFile
    if commands.nonEmpty && !file.exists then
      try FileUtils.writeLines(file, StandardCharsets.UTF_8.name, asJava(commands))
      catch
        case ex: IOException => Logger.error("Could not write REPL initial commands file", ex)

  def initialReplCommandsFileExists(@NotNull module: Module): Boolean =
    Paths
      .get(getModuleDirectory(module), PluginSettings.MODULE_REPL_INITIAL_COMMANDS_FILE_NAME)
      .toFile
      .exists

  @NotNull
  def getInitialReplCommands(module: Module): List[String] =
      PluginSettings
        .getMainViewModel(module.getProject)
        .courseViewModel
        .map(_.model.replInitialCommands.getOrElse(module.getName, List()))
        .getOrElse(List())

  def isTopLevelModule(module: Module): Boolean = module.getName.equals(module.getProject.getName)

  def isScala3Module(module: Module): Boolean = nonEmpty(
    ModuleRootManager.getInstance(module)
      .orderEntries()
      .librariesOnly()
      .satisfying(x => x.getPresentableName.contains("scala3-") || x.getPresentableName.contains("scala-sdk-3."))
  )
