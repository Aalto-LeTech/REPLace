package fi.aalto.cs.replace

/** Decides how much of each REPL output chunk can be printed now, so that output and the prompts
  * below it read as consecutive lines however the console chunks the stream. Every prompt consumes
  * exactly one spacing newline, the blank line dotty prints above it, and chunk-final text that
  * cannot be classified yet is held back until the next chunk.
  *
  * Not thread-safe. The caller must keep each classify-and-print pair in one critical section.
  */
private[replace] final class ReplSpacingFilter(promptToken: String):
  private var heldBackTail       = ""
  private var endsWithNewline    = true
  private var heldCarriageReturn = false

  /** Normalizes CRLF pairs into newlines while preserving bare carriage returns. A chunk-final `\r`
    * is held for the next chunk, which may complete the pair; releasing it split would print a
    * cursor return of its own.
    */
  def normalize(chunk: String): String =
    val joined = (if heldCarriageReturn then "\r" else "") + chunk
    heldCarriageReturn = joined.endsWith("\r")
    (if heldCarriageReturn then joined.dropRight(1) else joined).replace("\r\n", "\n")

  /** Classifies one already-normalized chunk into what reaches the console, which the caller must
    * then print exactly, within one critical section. `promptHidden` drops a prompt that only
    * released the next paste group, and `suppressBlank` hides whitespace while startup commands are
    * fed.
    */
  def render(
      text: String,
      promptCompleted: Boolean,
      promptHidden: Boolean,
      suppressBlank: Boolean
  ): ReplSpacingFilter.Rendered =
    val displayText = join(text)
    val promptPart =
      if promptCompleted && displayText.endsWith(promptToken) then promptToken else ""
    // The chunk either ends at a prompt, whose spacing is resolved here and now, or ends in text
    // that the next chunk may still turn into one.
    val body =
      if promptPart.isEmpty then withoutAmbiguousTail(displayText)
      else beforePrompt(displayText.dropRight(promptPart.length))
    ReplSpacingFilter.Rendered(
      body = if suppressBlank && body.trim.isEmpty then "" else body,
      prompt = if promptHidden || suppressBlank then "" else promptPart
    )

  private def join(text: String): String =
    val joined = heldBackTail + text
    heldBackTail = ""
    joined

  /** The body above a prompt, with the prompt's single spacing newline consumed. User blank lines
    * beyond it are kept, and the terminator of a still-open line is never consumed as spacing.
    */
  private def beforePrompt(body: String): String =
    val spacing = trailingNewlineCount(body)
    val content = body.dropRight(spacing)
    val kept    = math.max(spacing - 1, 0)
    val core    = content + ("\n" * kept)
    if kept == 0 && lineOpenBefore(content) then core + "\n" else core

  /** The body without its unclassifiable tail, meaning at most one trailing newline and any
    * chunk-final prefix of the prompt at a line start. Only the last line is inspected, which is
    * correct because `promptToken` contains no newline. The tail is stored for [[join]] to reclaim.
    */
  private def withoutAmbiguousTail(body: String): String =
    val lineStart = body.lastIndexOf('\n') + 1
    val lastLine  = body.substring(lineStart)
    val promptPrefix =
      if lastLine.nonEmpty && lastLine.length < promptToken.length &&
        promptToken.startsWith(lastLine) && (lineStart > 0 || endsWithNewline)
      then lastLine
      else ""
    val beforePrefix     = body.dropRight(promptPrefix.length)
    val trailingNewlines = trailingNewlineCount(beforePrefix)
    val holdNewline =
      trailingNewlines >= 2 ||
        (trailingNewlines == 1 && !lineOpenBefore(beforePrefix.dropRight(1)))
    val heldNewlines = if holdNewline then 1 else 0
    heldBackTail = ("\n" * heldNewlines) + promptPrefix
    beforePrefix.dropRight(heldNewlines)

  /** Whether a line is still open before a chunk's trailing newlines, which makes the first of them
    * that line's terminator rather than spacing.
    */
  private def lineOpenBefore(content: String): Boolean =
    content.nonEmpty || !endsWithNewline

  /** Records text that actually reached the console, keeping the end-of-line state in step. */
  def recordPrinted(text: String): Unit =
    endsWithNewline = text.endsWith("\n")

  /** Everything still withheld, for printing when no further chunk will reclaim it. `heldByCaller`
    * is the caller's own withheld text, which follows this filter's tail and precedes a chunk-final
    * carriage return. A caller that has already normalized the chunk that carriage return came from
    * keeps it held, because that chunk's own body has not been printed yet. Resets what it returns.
    */
  def flush(heldByCaller: String = "", keepCarriageReturn: Boolean = false): String =
    val released = heldCarriageReturn && !keepCarriageReturn
    val leftover = heldBackTail + heldByCaller + (if released then "\r" else "")
    heldBackTail = ""
    heldCarriageReturn = heldCarriageReturn && !released
    leftover

  private def trailingNewlineCount(text: String): Int =
    text.length - (text.lastIndexWhere(_ != '\n') + 1)

private[replace] object ReplSpacingFilter:
  /** The printable body, and the chunk-final prompt to print separately (empty when hidden). */
  final case class Rendered(body: String, prompt: String)
