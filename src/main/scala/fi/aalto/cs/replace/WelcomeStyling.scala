package fi.aalto.cs.replace

import com.intellij.execution.ui.ConsoleViewContentType
import com.intellij.openapi.editor.DefaultLanguageHighlighterColors
import com.intellij.openapi.editor.colors.EditorColorsManager
import com.intellij.openapi.editor.markup.TextAttributes
import com.intellij.util.ui.UIUtil

import java.awt.Font

/** Styles the plugin's rewritten welcome text in the console history. `ScalaLanguageConsole`
  * coerces everything printed through its two-argument `print` during its welcome phase to one bold
  * content type, so the caller prints the segments through `ConsoleViewImpl`'s three-argument
  * `print`, which nothing in between overrides, one content type per style.
  */
private[replace] object WelcomeStyling:

  private[replace] enum WelcomeStyle:
    case Body, Muted, Module

  /** The content type a style is printed with. */
  private[replace] def contentTypeFor(style: WelcomeStyle): ConsoleViewContentType = style match
    case WelcomeStyle.Body   => ConsoleViewContentType.NORMAL_OUTPUT
    case WelcomeStyle.Muted  => mutedContentType
    case WelcomeStyle.Module => moduleContentType

  /** Plain text in the color of a hint. The color is a `JBColor`, so it follows the theme. */
  private lazy val mutedContentType: ConsoleViewContentType =
    val attributes = new TextAttributes()
    attributes.setForegroundColor(UIUtil.getContextHelpForeground)
    attributes.setFontType(Font.PLAIN)
    new ConsoleViewContentType("REPLACE_WELCOME_MUTED", attributes)

  /** The scheme's class-name color in bold, for the module name in the greeting. Read from the
    * scheme in force when the first REPL of the session prints its welcome.
    */
  private lazy val moduleContentType: ConsoleViewContentType =
    val scheme = EditorColorsManager.getInstance.getGlobalScheme
    val attributes = Option(scheme.getAttributes(DefaultLanguageHighlighterColors.CLASS_NAME))
      .map(_.clone())
      .getOrElse(new TextAttributes())
    attributes.setForegroundColor(
      Option(attributes.getForegroundColor).getOrElse(scheme.getDefaultForeground)
    )
    attributes.setFontType(Font.BOLD)
    new ConsoleViewContentType("REPLACE_WELCOME_MODULE", attributes)

  /** Splits the welcome text into consecutive (text, style) segments that reconstruct it exactly.
    * The styled parts are the greeting line with the module name picked out, the labels of the
    * indented quick-reference entries, and the auto-import summary. Everything else is
    * [[WelcomeStyle.Body]].
    */
  private[replace] def welcomeSegments(
      welcomeText: String,
      moduleName: String
  ): Seq[(String, WelcomeStyle)] =
    var previousLineWasBlank = false
    welcomeText.linesWithSeparators.toSeq.zipWithIndex
      .flatMap { (rawLine, lineIndex) =>
        val line      = rawLine.stripLineEnd
        val separator = rawLine.substring(line.length)
        val pieces =
          if lineIndex == 0 then greetingPieces(line, moduleName)
          else if line.startsWith("  ") then labelPieces(line, line.indexOf(':', 2), labelStart = 2)
          else if line.nonEmpty && previousLineWasBlank then Seq(line -> WelcomeStyle.Muted)
          else labelPieces(line, line.indexOf(':'), labelStart = 0)
        previousLineWasBlank = line.isEmpty
        pieces :+ (separator -> WelcomeStyle.Body)
      }
      .filter(_._1.nonEmpty)

  /** The greeting line, muted with the module name picked out. */
  private def greetingPieces(line: String, moduleName: String): Seq[(String, WelcomeStyle)] =
    line.lastIndexOf(moduleName) match
      case -1 => Seq(line -> WelcomeStyle.Muted)
      case start =>
        Seq(
          line.take(start)                     -> WelcomeStyle.Muted,
          moduleName                           -> WelcomeStyle.Module,
          line.drop(start + moduleName.length) -> WelcomeStyle.Muted
        )

  /** A line with a muted label ending at `colon`, or all body text when there is no colon. */
  private def labelPieces(line: String, colon: Int, labelStart: Int): Seq[(String, WelcomeStyle)] =
    if colon < 0 then Seq(line -> WelcomeStyle.Body)
    else
      Seq(
        line.take(labelStart)             -> WelcomeStyle.Body,
        line.slice(labelStart, colon + 1) -> WelcomeStyle.Muted,
        line.drop(colon + 1)              -> WelcomeStyle.Body
      )
