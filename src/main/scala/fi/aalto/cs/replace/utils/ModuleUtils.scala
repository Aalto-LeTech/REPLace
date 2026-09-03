package fi.aalto.cs.replace.utils

import com.intellij.ide.DataManager
import com.intellij.ide.projectView.ProjectView
import com.intellij.openapi.actionSystem.{CommonDataKeys, DataContext, PlatformCoreDataKeys}
import com.intellij.openapi.application.ReadAction
import com.intellij.openapi.fileEditor.FileDocumentManager
import com.intellij.openapi.keymap.KeymapUtil
import com.intellij.openapi.module.{Module, ModuleManager, ModuleUtilCore}
import com.intellij.openapi.project.{Project, ProjectUtil}
import com.intellij.openapi.util.io.FileUtilRt
import com.intellij.util.concurrency.annotations.RequiresReadLock
import org.jetbrains.plugins.scala.project.ModuleExt

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}
import java.util.concurrent.Callable

object ModuleUtils:
  private val initialCommandsFileName = ".repl-commands"

  /** The module's content root when it can be guessed, falling back to the `.iml` directory. The
    * guess must come first, because Gradle and sbt imports put `.iml` files under `.idea/modules`,
    * which is neither the REPL's working directory nor where course files live.
    */
  @RequiresReadLock
  def getModuleDirectory(module: Module): String =
    val path = Option(ProjectUtil.guessModuleDir(module))
      .map(_.getPath) // VFS paths are always '/'-separated
      .getOrElse(ModuleUtilCore.getModuleDirPath(module))
    FileUtilRt.toSystemIndependentName(path)

  /** Finds the module a new REPL should be started for.
    *
    * The priority order is:
    *   1. The module of the file open in the editor.
    *   1. The module selected in the project tree.
    *   1. The module of the file selected in the project tree.
    *   1. The same three rules against the project tree's own data context, which reports the
    *      selection whether or not the tree has focus.
    *   1. The project's only eligible module.
    *
    * Ineligible candidates fall through to the next rule. Two or more eligible modules that no rule
    * picks out leave the choice to the caller, which has [[eligibleModules]] for it.
    */
  def getScalaReplModule(
      project: Project,
      dataContext: DataContext,
      isEligible: Module => Boolean
  ): Option[Module] =
    moduleFromContext(project, dataContext, isEligible)
      .orElse(projectViewContext(project).flatMap(moduleFromContext(project, _, isEligible)))
      .orElse {
        val eligible = eligibleModules(project, isEligible)
        Option.when(eligible.sizeIs == 1)(eligible.head)
      }

  /** Every module of the project a REPL can be started for, by name. `getModules` has no documented
    * order, and a course project can hold a whole term's worth of downloaded modules.
    */
  def eligibleModules(
      project: Project,
      isEligible: Module => Boolean
  ): List[Module] =
    ModuleManager.getInstance(project).getModules.filter(isEligible).sortBy(_.getName).toList

  /** The first three rules against one data context. */
  private[replace] def moduleFromContext(
      project: Project,
      dataContext: DataContext,
      isEligible: Module => Boolean
  ): Option[Module] =
    getModuleOfEditorFile(project, dataContext)
      .filter(isEligible)
      .orElse(Option(PlatformCoreDataKeys.MODULE.getData(dataContext)).filter(isEligible))
      .orElse(getModuleOfSelectedFile(project, dataContext).filter(isEligible))

  /** The data context of the project tree, which carries the tree's selection no matter which
    * component has focus. There is none until the tool window has been created.
    */
  private def projectViewContext(project: Project): Option[DataContext] =
    Option(ProjectView.getInstance(project))
      .flatMap(view => Option(view.getCurrentProjectViewPane))
      .flatMap(pane => Option(pane.getTree))
      .map(tree => DataManager.getInstance.getDataContext(tree))

  private def getModuleOfEditorFile(project: Project, dataContext: DataContext): Option[Module] =
    Option(CommonDataKeys.EDITOR.getData(dataContext))
      .flatMap(editor => Option(FileDocumentManager.getInstance.getFile(editor.getDocument)))
      .flatMap(openFile => Option(ModuleUtilCore.findModuleForFile(openFile, project)))

  private def getModuleOfSelectedFile(project: Project, dataContext: DataContext): Option[Module] =
    Option(CommonDataKeys.VIRTUAL_FILE.getData(dataContext))
      .flatMap(file => Option(ModuleUtilCore.findModuleForFile(file, project)))

  // O1_SPECIFIC
  /** Both wildcard syntaxes, because a Scala 2 course module imports with `_`. */
  private[replace] def isO1WildcardImport(command: String): Boolean =
    command.matches("import\\s+o1(\\.[\\w$]+)*\\.[*_]")

  private[replace] def autoImportSummary(imports: List[String]): String =
    if imports.isEmpty then ""
    else MyBundle.message("ui.repl.console.welcome.autoImport.message", imports.mkString(", "))

  /** The console's welcome text: the shortcut reference around the REPL's own `replWelcomeLine`, or
    * the course greeting with its auto-import summary inside an A+ project.
    */
  def welcomeText(
      module: Module,
      commands: List[String],
      replWelcomeLine: String,
      isCoursesProject: Boolean
  ): String =
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
      // Outside an A+ course project nothing here concerns course modules.
      commonText + "\n" + replWelcomeLine
    else if isTopLevelModule(module) then
      MyBundle.message(
        "ui.repl.console.welcome.noModuleText",
        commonText,
        replWelcomeLine,
        getPrettyKeyMapString("Scala.RunConsole")
      )
    else
      val imports = commands
        .filter(isO1WildcardImport)
        .map(_.replaceAll("import\\s+", "").stripSuffix(".*").stripSuffix("._"))
      formatCourseWelcome(module.getName, autoImportSummary(imports), commonText)

  private[replace] def formatCourseWelcome(
      moduleName: String,
      commandsText: String,
      commonText: String
  ): String =
    MyBundle.message(
      "ui.repl.console.welcome.fullText",
      moduleName,
      // The summary carries its own line break, so an empty one collapses to one blank line.
      if commandsText.isEmpty then "" else commandsText + "\n",
      commonText
    )

  /** The active keymap's rendering of an action's first shortcut, macOS glyphs included. The
    * platform returns an empty string when nothing is bound.
    */
  private def getPrettyKeyMapString(actionId: String): String =
    val shortcut = KeymapUtil.getFirstKeyboardShortcutText(actionId)
    if shortcut.nonEmpty then shortcut
    else MyBundle.message("ui.repl.console.welcome.shortcutMissing")

  /** The module's startup-commands file, when it is there to be read. Resolved from
    * [[getModuleDirectory]] so the Scala 2 `-i` argument and the Scala 3 `:load` agree.
    */
  @RequiresReadLock
  private[replace] def initialCommandsFile(module: Module): Option[Path] =
    val file = Path.of(getModuleDirectory(module), initialCommandsFileName)
    Option.when(Files.isRegularFile(file) && Files.isReadable(file))(file)

  /** The same file, resolved under the read action root-model access needs. The non-blocking read
    * action suits both callers without a thread test: it runs the computation directly on a thread
    * that already has read access, and lets a pending write action cancel and retry it on one that
    * does not.
    */
  def getInitialReplCommandsFile(module: Module): Option[Path] =
    val resolve: Callable[Option[Path]] = () => initialCommandsFile(module)
    ReadAction.nonBlocking(resolve).executeSynchronously()

  /** The commands in the file, for the welcome text's summary. Undecodable bytes become replacement
    * characters rather than an exception, and a byte order mark is not part of the first command.
    */
  def getInitialReplCommands(file: Path): List[String] =
    new String(Files.readAllBytes(file), UTF_8).stripPrefix("\uFEFF").linesIterator.toList

  private def isTopLevelModule(module: Module): Boolean =
    module.getName == module.getProject.getName

  def isScala3Module(module: Module): Boolean = module.hasScala3
