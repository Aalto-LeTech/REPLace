package fi.aalto.cs.replace

import com.intellij.ide.DataManager
import com.intellij.ide.impl.HeadlessDataManager
import com.intellij.openapi.actionSystem.impl.SimpleDataContext
import com.intellij.openapi.actionSystem.{
  CommonDataKeys,
  DataContext,
  DataSink,
  LangDataKeys,
  PlatformCoreDataKeys,
  UiDataProvider
}
import com.intellij.openapi.application.WriteAction
import com.intellij.openapi.editor.{Editor, EditorFactory}
import com.intellij.openapi.fileEditor.FileDocumentManager
import com.intellij.openapi.keymap.KeymapUtil
import com.intellij.openapi.module.Module
import com.intellij.openapi.roots.ModuleRootManager
import com.intellij.openapi.util.Disposer
import com.intellij.openapi.vfs.VirtualFile
import fi.aalto.cs.replace.utils.ModuleUtils
import fi.aalto.cs.replace.utils.MyBundle.message
import org.junit.Assert.{assertEquals, assertFalse, assertTrue}
import org.junit.jupiter.api.Test

import java.io.IOException
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.Files
import javax.swing.JTree

class ModuleUtilsTest extends ReplPlatformTestBase:

  @Test
  def testModuleSelectedInProjectTreeIsUsed(): Unit =
    // A module node in the project tree provides the module directly (issue #10).
    val context = SimpleDataContext.getSimpleContext(PlatformCoreDataKeys.MODULE, getModule)
    assertEquals(
      Some(getModule),
      ModuleUtils.getScalaReplModule(getProject, context, isEligible = _ => true)
    )

  @Test
  def testTheModuleOfTheOpenFileOutranksTheSelectedModule(): Unit =
    // The first rule of the documented order: what the user is looking at wins over what the
    // project tree happens to have selected.
    val file  = createFileInTheModule("Open.scala")
    val other = createSecondModule()
    withEditorFor(file) { editor =>
      val context = SimpleDataContext
        .builder()
        .add(CommonDataKeys.EDITOR, editor)
        .add(PlatformCoreDataKeys.MODULE, other)
        .add(CommonDataKeys.VIRTUAL_FILE, file)
        .build()

      assertEquals(
        Some(getModule),
        ModuleUtils.getScalaReplModule(getProject, context, isEligible = _ => true)
      )
    }

  @Test
  def testTheSelectedModuleOutranksTheSelectedFile(): Unit =
    // The remaining two rules: a selected module node beats a selected file, and the file's own
    // module is used when nothing else identifies one.
    val file    = createFileInTheModule("Selected.scala")
    val other   = createSecondModule()
    val builder = SimpleDataContext.builder().add(CommonDataKeys.VIRTUAL_FILE, file)

    assertEquals(
      Some(other),
      ModuleUtils.getScalaReplModule(
        getProject,
        builder.add(PlatformCoreDataKeys.MODULE, other).build(),
        isEligible = _ => true
      )
    )
    assertEquals(
      Some(getModule),
      ModuleUtils.getScalaReplModule(
        getProject,
        SimpleDataContext.getSimpleContext(CommonDataKeys.VIRTUAL_FILE, file),
        isEligible = _ => true
      )
    )

  @Test
  def testTheProjectTreesSelectionIsReadWithoutFocus(): Unit =
    // The fix for issue #2: the rules are run again over the data context of the project tree's
    // component, which reports its selection whichever component holds focus. The light fixture
    // never builds a project view pane, so a tree that carries the same selection data stands in
    // for it; the headless data manager ignores components until it is told not to.
    val other      = createSecondModule()
    val disposable = Disposer.newDisposable()
    try
      HeadlessDataManager.fallbackToProductionDataManager(disposable)
      val project = getProject
      val tree = new JTree with UiDataProvider:
        override def uiDataSnapshot(sink: DataSink): Unit =
          sink.set(CommonDataKeys.PROJECT, project)
          sink.set(LangDataKeys.MODULE_CONTEXT, other)

      assertEquals(
        Some(other),
        ModuleUtils.moduleFromContext(
          project,
          DataManager.getInstance.getDataContext(tree),
          isEligible = _ => true
        )
      )
    finally Disposer.dispose(disposable)

  @Test
  def testTheOnlyEligibleModuleIsUsedWhenNothingIsSelected(): Unit =
    // With one candidate there is nothing to disambiguate, so an empty context still starts a REPL
    // for the module the project has.
    assertEquals(
      Some(getModule),
      ModuleUtils.getScalaReplModule(getProject, DataContext.EMPTY_CONTEXT, isEligible = _ => true)
    )

  @Test
  def testSeveralEligibleModulesLeaveTheChoiceToTheCaller(): Unit =
    val other = createSecondModule()
    assertEquals(
      None,
      ModuleUtils.getScalaReplModule(getProject, DataContext.EMPTY_CONTEXT, isEligible = _ => true)
    )
    // By name, because the chooser lists them in exactly this order.
    assertEquals(
      List(getModule, other).sortBy(_.getName),
      ModuleUtils.eligibleModules(getProject, isEligible = _ => true)
    )

  @Test
  def testIneligibleModulesAreSkipped(): Unit =
    val context = SimpleDataContext.getSimpleContext(PlatformCoreDataKeys.MODULE, getModule)
    assertEquals(
      None,
      ModuleUtils.getScalaReplModule(getProject, context, isEligible = _ => false)
    )

  @Test
  def testRecognizesO1WildcardImports(): Unit =
    assertTrue(ModuleUtils.isO1WildcardImport("import o1.*"))
    assertTrue(ModuleUtils.isO1WildcardImport("import o1.goodstuff.gui.*"))
    // A Scala 2 course module imports with `_`, and a package name may carry a digit.
    assertTrue(ModuleUtils.isO1WildcardImport("import o1._"))
    assertTrue(ModuleUtils.isO1WildcardImport("import o1.week2.*"))
    assertFalse(ModuleUtils.isO1WildcardImport("import scala.collection.*"))
    assertFalse(ModuleUtils.isO1WildcardImport("import o1.train"))

  @Test
  def testTheAutoImportSummaryCoversBothWildcardSyntaxes(): Unit =
    val welcomeText = ModuleUtils.welcomeText(
      getModule,
      List("import o1._", "import o1.week2.*"),
      "",
      isCoursesProject = true
    )

    assertTrue(
      welcomeText,
      welcomeText.contains(message("ui.repl.console.welcome.autoImport.message", "o1, o1.week2"))
    )

  @Test
  def testStartupCommandsDropAByteOrderMarkAndDecodeLeniently(): Unit =
    // A file re-saved by a Windows editor can start with a byte order mark, which would otherwise
    // become part of the first command, and one undecodable byte must not cost the whole file.
    val file = Files.createTempFile("replace-commands", ".txt")
    try
      Files.write(file, "\uFEFFimport o1.*\n".getBytes(UTF_8) ++ Array[Byte](0xe4.toByte, '\n'))
      assertEquals(List("import o1.*", "\uFFFD"), ModuleUtils.getInitialReplCommands(file))
    finally Files.deleteIfExists(file)

  @Test
  def testSummarizesAutoImports(): Unit =
    assertEquals("", ModuleUtils.autoImportSummary(List.empty))
    assertEquals("Auto-imported: o1", ModuleUtils.autoImportSummary(List("o1")))
    assertEquals(
      "Auto-imported: o1, o1.goodstuff",
      ModuleUtils.autoImportSummary(List("o1", "o1.goodstuff"))
    )

  @Test
  def testCourseWelcomeTextWithoutAutoImportsHasNoDoubleBlankLine(): Unit =
    // autoImportSummary returns "" for a module with no auto-imports; the layout must then collapse
    // to a single blank line before the quick reference rather than leaving a gap.
    val welcomeText = ModuleUtils.formatCourseWelcome(
      "IntroApps",
      "",
      """Quick reference
        |  Run code:       Ctrl+Enter
        |""".stripMargin
    )

    assertEquals(
      """Loaded A+ Courses module: IntroApps
        |
        |Quick reference
        |  Run code:       Ctrl+Enter
        |""".stripMargin,
      welcomeText
    )

  @Test
  def testCourseWelcomeTextHasAScannableLayout(): Unit =
    val welcomeText = ModuleUtils.formatCourseWelcome(
      "IntroApps",
      "Auto-imported: o1, o1.goodstuff",
      """Quick reference
        |  Run code:       Ctrl+Enter
        |  Browse history: Up / Down
        |  Restart REPL:   Ctrl+F5 or the toolbar icon
        |""".stripMargin
    )

    assertEquals(
      """Loaded A+ Courses module: IntroApps
        |Auto-imported: o1, o1.goodstuff
        |
        |Quick reference
        |  Run code:       Ctrl+Enter
        |  Browse history: Up / Down
        |  Restart REPL:   Ctrl+F5 or the toolbar icon
        |""".stripMargin,
      welcomeText
    )

    // The styling is derived from that layout, so it is asserted against the very same text: the
    // greeting and the entry labels are muted, and the module name is picked out.
    assertEquals(
      Seq(
        WelcomeStyling.WelcomeStyle.Muted  -> "Loaded A+ Courses module: ",
        WelcomeStyling.WelcomeStyle.Module -> "IntroApps",
        WelcomeStyling.WelcomeStyle.Muted  -> "Auto-imported:",
        WelcomeStyling.WelcomeStyle.Muted  -> "Quick reference",
        WelcomeStyling.WelcomeStyle.Muted  -> "Run code:",
        WelcomeStyling.WelcomeStyle.Muted  -> "Browse history:",
        WelcomeStyling.WelcomeStyle.Muted  -> "Restart REPL:"
      ),
      styledSegments(welcomeText, "IntroApps")
    )

  @Test
  def testTheCourseWelcomeNamesTheModuleAndItsWildcardImports(): Unit =
    // The courses branch for a module of its own: the quick reference, the module name, and a
    // summary of the o1 wildcard imports. A plain import is loaded but not advertised.
    val welcomeText = ModuleUtils.welcomeText(
      getModule,
      List("import o1.*", "import o1.goodstuff.*", "import scala.collection.mutable.Buffer"),
      "",
      isCoursesProject = true
    )

    val commonText = message(
      "ui.repl.console.welcome.commonText",
      shortcutOf("ScalaConsole.Execute"),
      shortcutOf("EditorUp"),
      shortcutOf("EditorDown"),
      shortcutOf("Rerun")
    ) + "\n"
    assertEquals(
      message(
        "ui.repl.console.welcome.fullText",
        getModule.getName,
        message("ui.repl.console.welcome.autoImport.message", "o1, o1.goodstuff") + "\n",
        commonText
      ),
      welcomeText
    )

  @Test
  def testWelcomeOutsideACourseProjectDoesNotMentionCourses(): Unit =
    val welcomeText =
      ModuleUtils.welcomeText(getModule, List("import o1.*"), "", isCoursesProject = false)

    assertFalse(welcomeText, welcomeText.contains("A+"))
    assertFalse(welcomeText, welcomeText.contains("course module"))
    assertTrue(welcomeText, welcomeText.contains("Quick reference"))

  @Test
  def testWelcomeSegmentsSurviveWindowsLineEndings(): Unit =
    // A line terminator of a different length must not shift the pieces; reconstruction (inside
    // styledSegments) is what proves they stayed aligned.
    val welcomeText =
      "Loaded A+ Courses module: IntroApps\r\n\r\nQuick reference\r\n  Run code:       Ctrl+Enter\r\n"
    assertEquals(
      Seq(
        WelcomeStyling.WelcomeStyle.Muted  -> "Loaded A+ Courses module: ",
        WelcomeStyling.WelcomeStyle.Module -> "IntroApps",
        WelcomeStyling.WelcomeStyle.Muted  -> "Quick reference",
        WelcomeStyling.WelcomeStyle.Muted  -> "Run code:"
      ),
      styledSegments(welcomeText, "IntroApps")
    )

  /** A file under the module's content root, which is what resolves it to that module. */
  private def createFileInTheModule(name: String): VirtualFile =
    val root = ModuleRootManager.getInstance(getModule).getContentRoots.head
    WriteAction.compute[VirtualFile, IOException](() => root.createChildData(this, name))

  private def withEditorFor(file: VirtualFile)(test: Editor => Unit): Unit =
    val factory  = EditorFactory.getInstance
    val document = FileDocumentManager.getInstance.getDocument(file)
    val editor   = factory.createEditor(document, getProject)
    try test(editor)
    finally factory.releaseEditor(editor)

  /** The active keymap's rendering of a shortcut, with the fallback for an unbound action. The
    * welcome text is assembled from exactly these.
    */
  private def shortcutOf(actionId: String): String =
    val shortcut = KeymapUtil.getFirstKeyboardShortcutText(actionId)
    if shortcut.nonEmpty then shortcut else message("ui.repl.console.welcome.shortcutMissing")

  /** Asserts the segments reconstruct `welcomeText`, then returns the styled pairs to pin. */
  private def styledSegments(
      welcomeText: String,
      moduleName: String
  ): Seq[(WelcomeStyling.WelcomeStyle, String)] =
    val segments = WelcomeStyling.welcomeSegments(welcomeText, moduleName)
    assertEquals(welcomeText, segments.map(_._1).mkString)
    segments.collect {
      case (text, style) if style != WelcomeStyling.WelcomeStyle.Body => style -> text
    }
