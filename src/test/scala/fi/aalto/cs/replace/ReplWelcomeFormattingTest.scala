package fi.aalto.cs.replace

import com.intellij.execution.ui.ConsoleViewContentType
import com.intellij.openapi.editor.markup.TextAttributes
import com.intellij.openapi.util.Disposer
import com.intellij.testFramework.fixtures.BasePlatformTestCase
import org.junit.Assert.assertTrue

import scala.collection.mutable

class ReplWelcomeFormattingTest extends BasePlatformTestCase:

  /** The segments themselves are covered by [[ModuleUtilsTest]]. What this test checks is the
    * wiring. Once the printed welcome text is flushed into the history viewer, its segments are
    * styled with several different attributes rather than left as one uniform block.
    */
  def testWelcomeTextIsStyledInSeveralStyles(): Unit =
    val repl = new Repl(getModule):
      override private[replace] def initialCommands: List[String] = List.empty
      override private[replace] def scala3Module: Boolean         = false

    try
      // In the IDE the run-content UI initializes the console component; without it the console
      // has no editor to flush the printed text into.
      repl.getComponent()
      repl.print(Repl.welcomeLine, ConsoleViewContentType.NORMAL_OUTPUT)
      repl.flushDeferredText()

      val editor = repl.getHistoryViewer
      assertTrue(
        s"welcome text was not rewritten; document was ${editor.getDocument.getText}",
        editor.getDocument.getText.contains("Quick reference")
      )

      val attributes = mutable.LinkedHashSet.empty[TextAttributes]
      editor.getMarkupModel.getAllHighlighters.foreach { highlighter =>
        Option(highlighter.getTextAttributes(editor.getColorsScheme)).foreach(attributes += _)
      }

      assertTrue(
        s"expected the welcome text to be printed in several styles, got ${attributes.size}",
        attributes.size > 1
      )
    finally Disposer.dispose(repl)
