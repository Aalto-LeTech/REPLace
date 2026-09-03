package fi.aalto.cs.replace

import com.intellij.execution.ui.ConsoleViewContentType
import com.intellij.openapi.editor.impl.DocumentMarkupModel
import com.intellij.openapi.editor.markup.TextAttributes
import org.junit.Assert.{assertEquals, assertNotSame, assertSame, assertTrue}
import org.junit.jupiter.api.Test

import java.nio.file.{Files, Path}

class ReplWelcomeFormattingTest extends ReplPlatformTestBase:

  private def scala2Repl: Repl =
    new Repl(getModule):
      override private[replace] def initialCommandsFile: Option[Path] = None
      override private[replace] def scala3Module: Boolean             = false

  private def flushedHistory(repl: Repl): String =
    repl.flushDeferredText()
    repl.getHistoryViewer.getDocument.getText

  private def occurrences(text: String, part: String): Int =
    text.sliding(part.length).count(_ == part)

  /** Writes the marker file that makes [[Repl]] treat this project as an A+ course project, whose
    * welcome names the module. Must run before the console is built, which reads the marker once.
    */
  private def markProjectAsACoursesProject(): Unit =
    val ideaDirectory = Path.of(getProject.getBasePath, ".idea")
    Files.createDirectories(ideaDirectory)
    Files.writeString(ideaDirectory.resolve("aplus_project.xml"), "<project/>")

  /** The attributes the console gave the character at `offset`. Each printed token becomes one
    * range highlighter of the document's markup model, carrying its content type's attributes.
    */
  private def attributesAt(repl: Repl, offset: Int): TextAttributes =
    val editor = repl.getHistoryViewer
    val attributes = DocumentMarkupModel
      .forDocument(editor.getDocument, getProject, true)
      .getAllHighlighters
      .find(highlighter =>
        highlighter.getStartOffset <= offset && offset < highlighter.getEndOffset
      )
      .flatMap(highlighter => Option(highlighter.getTextAttributes(editor.getColorsScheme)))
    assertTrue(s"nothing was printed at offset $offset", attributes.isDefined)
    attributes.get

  /** [[ModuleUtilsTest]] covers the segments. This checks the wiring, that the flushed welcome text
    * carries one content type per style rather than the single one the Scala console coerces to.
    */
  @Test
  def testWelcomeTextIsPrintedInSeveralContentTypes(): Unit =
    markProjectAsACoursesProject()
    withRepl(scala2Repl) { (repl, _) =>
      repl.print(Repl.welcomeLine, ConsoleViewContentType.NORMAL_OUTPUT)
      val history    = flushedHistory(repl)
      val moduleName = getModule.getName

      assertTrue(
        s"the course welcome must name the module; document was $history",
        history.contains(s"Loaded A+ Courses module: $moduleName")
      )

      val greeting = attributesAt(repl, history.indexOf("Loaded"))
      val name     = attributesAt(repl, history.indexOf(moduleName))
      val indent   = attributesAt(repl, history.indexOf("  Run code:"))

      import WelcomeStyling.WelcomeStyle
      assertSame(
        "the greeting must be muted",
        WelcomeStyling.contentTypeFor(WelcomeStyle.Muted).getAttributes,
        greeting
      )
      assertSame(
        "the module name must be picked out",
        WelcomeStyling.contentTypeFor(WelcomeStyle.Module).getAttributes,
        name
      )
      assertNotSame("an entry's indent is body text, not muted", greeting, indent)
      assertNotSame("an entry's indent is body text, not the module name", name, indent)
    }

  @Test
  def testAWelcomeLineSplitAcrossChunksIsStillRewritten(): Unit =
    withRepl(scala2Repl) { (repl, _) =>
      repl.print("Type in expressions for eval", ConsoleViewContentType.NORMAL_OUTPUT)
      repl.print("uation. Or try :help.\n", ConsoleViewContentType.NORMAL_OUTPUT)
      val history = flushedHistory(repl)

      assertTrue(
        s"a split welcome line must still be rewritten; document was $history",
        history.contains("Quick reference")
      )
      assertEquals(
        s"the welcome line must not also appear truncated; document was $history",
        1,
        occurrences(history, "Type in expressions for eval")
      )
    }

  @Test
  def testAWelcomeLineMergedWithTheFollowingPromptIsStillRewritten(): Unit =
    withRepl(scala2Repl) { (repl, _) =>
      repl.print(Repl.welcomeLine + "\nscala> ", ConsoleViewContentType.NORMAL_OUTPUT)
      val history = flushedHistory(repl)

      assertTrue(
        s"a merged welcome line must still be rewritten; document was $history",
        history.contains("Quick reference")
      )
      assertTrue(
        s"the rest of the chunk must be printed as it arrived; document was $history",
        history.endsWith("scala> ")
      )
    }

  @Test
  def testTheFirstPromptFollowsTheWelcomeDirectly(): Unit =
    assertNoBlankLineBeforeTheFirstPrompt()

  @Test
  def testTheFirstPromptFollowsTheCourseWelcomeDirectly(): Unit =
    markProjectAsACoursesProject()
    assertNoBlankLineBeforeTheFirstPrompt()

  /** The Scala console prints a newline of its own when it leaves the system-output state for the
    * prompt, so the banner above the welcome must reach it through the printing path that moves it
    * into the welcome state instead.
    */
  private def assertNoBlankLineBeforeTheFirstPrompt(): Unit =
    withRepl(scala2Repl) { (repl, _) =>
      repl.print("java -cp . dotty.tools.repl.Main\n", ConsoleViewContentType.SYSTEM_OUTPUT)
      repl.print(
        s"Welcome to Scala 3.7.1\n${Repl.welcomeLine}\nscala> ",
        ConsoleViewContentType.NORMAL_OUTPUT
      )
      val history = flushedHistory(repl)

      assertTrue(
        s"the console must end at the prompt; document was $history",
        history.endsWith("scala> ")
      )
      assertEquals(
        s"nothing may separate the welcome from the prompt; document was $history",
        0,
        occurrences(history, "\n\nscala> ")
      )
    }
