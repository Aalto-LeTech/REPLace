package fi.aalto.cs.replace

import com.intellij.openapi.actionSystem.impl.SimpleDataContext
import com.intellij.openapi.actionSystem.{DataContext, PlatformCoreDataKeys}
import com.intellij.testFramework.fixtures.BasePlatformTestCase
import fi.aalto.cs.replace.utils.ModuleUtils
import org.junit.Assert.{assertEquals, assertFalse, assertTrue}

class ModuleUtilsTest extends BasePlatformTestCase:

  def testModuleSelectedInProjectTreeIsUsed(): Unit =
    // A module node in the project tree provides the module directly (issue #10).
    val context = SimpleDataContext.getSimpleContext(PlatformCoreDataKeys.MODULE, getModule)
    assertEquals(
      Some(getModule),
      ModuleUtils.getScalaReplModule(getProject, context, isEligible = _ => true)
    )

  def testNoModuleIsFoundInAnEmptyContext(): Unit =
    assertEquals(
      None,
      ModuleUtils.getScalaReplModule(getProject, DataContext.EMPTY_CONTEXT, isEligible = _ => true)
    )

  def testIneligibleModulesAreSkipped(): Unit =
    val context = SimpleDataContext.getSimpleContext(PlatformCoreDataKeys.MODULE, getModule)
    assertEquals(
      None,
      ModuleUtils.getScalaReplModule(getProject, context, isEligible = _ => false)
    )

  def testAcceptsTopLevelO1WildcardImport(): Unit =
    assertTrue(ModuleUtils.isO1WildcardImport("import o1.*"))

  def testAcceptsNestedO1WildcardImport(): Unit =
    assertTrue(ModuleUtils.isO1WildcardImport("import o1.goodstuff.gui.*"))

  def testRejectsNonO1Import(): Unit =
    assertFalse(ModuleUtils.isO1WildcardImport("import scala.collection.*"))

  def testRejectsNonWildcardImport(): Unit =
    assertFalse(ModuleUtils.isO1WildcardImport("import o1.train"))

  def testNoAutoImportsProduceNoSummary(): Unit =
    assertEquals("", ModuleUtils.getCommandsText(List.empty))

  def testSingleAutoImportIsSummarized(): Unit =
    assertEquals("Auto-imported: o1", ModuleUtils.getCommandsText(List("o1")))

  def testMultipleAutoImportsAreSummarizedCommaSeparated(): Unit =
    assertEquals(
      "Auto-imported: o1, o1.goodstuff",
      ModuleUtils.getCommandsText(List("o1", "o1.goodstuff"))
    )

  def testCourseWelcomeTextWithoutAutoImportsHasNoDoubleBlankLine(): Unit =
    // getCommandsText returns "" for a module with no auto-imports; the layout must then collapse
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
    val segments = Repl.welcomeSegments(welcomeText, "IntroApps")
    assertEquals(welcomeText, segments.map(_._1).mkString)
    assertEquals(
      Seq(
        Repl.WelcomeStyle.Muted  -> "Loaded A+ Courses module: ",
        Repl.WelcomeStyle.Module -> "IntroApps",
        Repl.WelcomeStyle.Muted  -> "Auto-imported:",
        Repl.WelcomeStyle.Muted  -> "Quick reference",
        Repl.WelcomeStyle.Muted  -> "Run code:",
        Repl.WelcomeStyle.Muted  -> "Browse history:",
        Repl.WelcomeStyle.Muted  -> "Restart REPL:"
      ),
      segments.collect { case (text, style) if style != Repl.WelcomeStyle.Body => style -> text }
    )

  def testWelcomeOutsideACourseProjectDoesNotMentionCourses(): Unit =
    val welcomeText =
      ModuleUtils.getUpdatedText(getModule, List("import o1.*"), "", isCoursesProject = false)

    assertFalse(welcomeText, welcomeText.contains("A+"))
    assertFalse(welcomeText, welcomeText.contains("course module"))
    assertTrue(welcomeText, welcomeText.contains("Quick reference"))

  def testWelcomeInsideACourseProjectKeepsTheCourseWording(): Unit =
    val welcomeText =
      ModuleUtils.getUpdatedText(getModule, List("import o1.*"), "", isCoursesProject = true)

    assertTrue(welcomeText, welcomeText.contains("course module") || welcomeText.contains("A+"))

  def testWelcomeSegmentsSurviveWindowsLineEndings(): Unit =
    // The segments are sliced with a running offset, so a line terminator of a different length
    // must not shift it. Reconstruction is what proves the offsets stayed aligned.
    val welcomeText =
      "Loaded A+ Courses module: IntroApps\r\n\r\nQuick reference\r\n  Run code:       Ctrl+Enter\r\n"
    val segments = Repl.welcomeSegments(welcomeText, "IntroApps")

    assertEquals(welcomeText, segments.map(_._1).mkString)
    assertEquals(
      Seq(
        Repl.WelcomeStyle.Muted  -> "Loaded A+ Courses module: ",
        Repl.WelcomeStyle.Module -> "IntroApps",
        Repl.WelcomeStyle.Muted  -> "Quick reference",
        Repl.WelcomeStyle.Muted  -> "Run code:"
      ),
      segments.collect { case (text, style) if style != Repl.WelcomeStyle.Body => style -> text }
    )
