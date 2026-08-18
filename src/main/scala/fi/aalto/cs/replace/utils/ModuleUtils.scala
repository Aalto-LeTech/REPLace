package fi.aalto.cs.replace.utils

import com.intellij.openapi.actionSystem.{CommonDataKeys, DataContext, PlatformCoreDataKeys}
import com.intellij.openapi.diagnostic.Logger
import com.intellij.openapi.fileEditor.FileDocumentManager
import com.intellij.openapi.keymap.KeymapManager
import com.intellij.openapi.module.{Module, ModuleUtilCore}
import com.intellij.openapi.project.{Project, ProjectUtil}
import com.intellij.openapi.util.io.FileUtilRt
import fi.aalto.cs.replace.Repl
import org.jetbrains.plugins.scala.project.ModuleExt

import java.io.IOException
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Paths}
import scala.io.Source
import scala.jdk.CollectionConverters.*

object ModuleUtils:
  private val logger = Logger.getInstance(ModuleUtils.getClass)

  def getModuleDirectory(module: Module): String =
    FileUtilRt.toSystemIndependentName(ModuleUtilCore.getModuleDirPath(module))

  /** Finds the module a new REPL should be started for.
    *
    * The priority order is:
    *   1. The module of the file open in the editor.
    *   1. The module selected in the project tree (issue #10).
    *   1. The module of the file selected in the project tree.
    *
    * Only modules accepted by `isEligible` (by default: those with a Scala SDK, to avoid the "no
    * Scala facet configured for module" error) are considered; other candidates fall through to the
    * next rule.
    */
  def getScalaReplModule(
      project: Project,
      dataContext: DataContext,
      isEligible: Module => Boolean = hasScalaSdkLibrary
  ): Option[Module] =
    getModuleOfEditorFile(project, dataContext)
      .filter(isEligible)
      .orElse(Option(PlatformCoreDataKeys.MODULE.getData(dataContext)).filter(isEligible))
      .orElse(getModuleOfSelectedFile(project, dataContext).filter(isEligible))

  def getModuleOfEditorFile(project: Project, dataContext: DataContext): Option[Module] =
    Option(CommonDataKeys.EDITOR.getData(dataContext))
      .flatMap(editor => Option(FileDocumentManager.getInstance.getFile(editor.getDocument)))
      .flatMap(openFile => Option(ModuleUtilCore.findModuleForFile(openFile, project)))

  def getModuleOfSelectedFile(project: Project, dataContext: DataContext): Option[Module] =
    Option(CommonDataKeys.VIRTUAL_FILE.getData(dataContext))
      .flatMap(file => Option(ModuleUtilCore.findModuleForFile(file, project)))

  // O1_SPECIFIC
  private[replace] def isO1WildcardImport(command: String): Boolean =
    command.matches("import\\s+o1(\\.[a-z]+)*\\.\\*")

  private def clearCommands(imports: List[String]): List[String] =
    imports
      .map(_.replace("import ", ""))
      .map(_.replace(".*", ""))

  private[replace] def getCommandsText(imports: List[String]): String =
    if imports.isEmpty then ""
    else MyBundle.message("ui.repl.console.welcome.autoImport.message", imports.mkString(", "))

  def getUpdatedText(
      module: Module,
      commands: List[String],
      originalText: String,
      isCoursesProject: Boolean
  ): String =
    val runConsoleShortCut     = getPrettyKeyMapString("Scala.RunConsole")
    val executeConsoleShortCut = getPrettyKeyMapString("ScalaConsole.Execute")
    val reRunShortCut          = getPrettyKeyMapString("Rerun")
    val editorUpShortCut       = getPrettyKeyMapString("EditorUp")
    val editorDownShortCut     = getPrettyKeyMapString("EditorDown")

    val commonText = MyBundle.message(
      "ui.repl.console.welcome.commonText",
      executeConsoleShortCut,
      editorUpShortCut,
      editorDownShortCut,
      reRunShortCut
    ) + "\n"

    if !isCoursesProject then
      // Outside an A+ course project the quick reference still helps, but nothing here has
      // anything to do with course modules.
      commonText + "\n" + originalText
    else if isTopLevelModule(module) then
      MyBundle.message(
        "ui.repl.console.welcome.noModuleText",
        commonText,
        originalText,
        runConsoleShortCut
      )
    else
      val validCommands   = commands.filter(isO1WildcardImport)
      val clearedCommands = clearCommands(validCommands)
      val commandsText    = getCommandsText(clearedCommands)

      formatCourseWelcome(module.getName, commandsText, commonText)

  private[replace] def formatCourseWelcome(
      moduleName: String,
      commandsText: String,
      commonText: String
  ): String =
    MyBundle.message(
      "ui.repl.console.welcome.fullText",
      moduleName,
      // The auto-import summary carries its own line break so that a module without any imports
      // collapses to a single blank line before the quick reference instead of two.
      if commandsText.isEmpty then "" else commandsText + "\n",
      commonText
    )

  private def getPrettyKeyMapString(actionId: String): String =
    val shortCuts = KeymapManager.getInstance.getActiveKeymap
      .getShortcuts(actionId)

    if shortCuts.nonEmpty then
      shortCuts.head.toString
        .replace("[", "")
        .replace("]", "")
        .split(" ")
        .filter(_ != "pressed")
        .map(_.toLowerCase)
        .map(_.capitalize)
        .mkString("+")
    else MyBundle.message("ui.repl.console.welcome.shortcutMissing")

  /** Creates the initial REPL commands file if it does not exist yet, otherwise does nothing.
    */
  def createInitialReplCommandsFile(module: Module): Unit =
    val commands = getInitialReplCommands(module)
    val file     = Paths.get(getModuleDirectory(module), Repl.initialCommandsFileName)
    if commands.nonEmpty && !Files.exists(file) then
      try Files.write(file, commands.asJava, UTF_8)
      catch case ex: IOException => logger.error("Could not write REPL initial commands file", ex)

  def initialReplCommandsFileExists(module: Module): Boolean =
    Files.exists(Paths.get(getModuleDirectory(module), Repl.initialCommandsFileName))

  def getInitialReplCommands(module: Module): List[String] =
    val commandsFile = Option(ProjectUtil.guessModuleDir(module)).flatMap { dir =>
      // The module dir may not be mappable to a real path (e.g. an in-memory filesystem),
      // in which case there is no commands file to read.
      try Some(dir.toNioPath.resolve(Repl.initialCommandsFileName).toFile)
      catch case _: UnsupportedOperationException => None
    }
    commandsFile.filter(file => file.exists && file.canRead) match
      case None => List()
      case Some(file) =>
        val source = Source.fromFile(file)
        try source.getLines().toList
        finally source.close

  private def isTopLevelModule(module: Module): Boolean =
    module.getName.equals(module.getProject.getName)

  def hasScalaSdkLibrary(module: Module): Boolean = module.hasScala

  def isScala3Module(module: Module): Boolean = module.hasScala3
