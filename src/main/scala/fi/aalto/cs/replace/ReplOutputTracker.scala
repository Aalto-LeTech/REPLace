package fi.aalto.cs.replace

/** Incrementally tracks REPL process output so that the prompt and the welcome line are recognized
  * even when the console delivers them split across, or merged into, arbitrary chunks.
  *
  * A prompt only counts when the stream currently ends with `"<prompt> "` at the start of a line
  * (or of the stream). Output that merely contains the prompt text mid-line, or a complete line
  * equal to the prompt, is not treated as a prompt.
  */
private[replace] final class ReplOutputTracker(promptText: String, welcomeLine: String):
  private val promptToken = promptText + " "
  private val maxKeptChars =
    math.max(512, promptToken.length + welcomeLine.length + 2)
  private var tail        = ""
  private var welcomeSeen = false

  /** Appends a chunk of process output and reports which stream events it completed. */
  def append(chunk: String): ReplOutputTracker.Events =
    if chunk.isEmpty then return ReplOutputTracker.Events(false, false)
    tail = (tail + chunk).takeRight(maxKeptChars)
    val welcomeCompleted = !welcomeSeen && tail.contains(welcomeLine)
    if welcomeCompleted then welcomeSeen = true
    ReplOutputTracker.Events(endsAtPrompt, welcomeCompleted)

  private def endsAtPrompt: Boolean =
    tail.endsWith(promptToken) && {
      val before = tail.dropRight(promptToken.length)
      before.isEmpty || before.endsWith("\n") || before.endsWith("\r")
      // An empty submission makes the REPL print the next prompt with nothing in between.
      || before.endsWith(promptToken)
    }

private[replace] object ReplOutputTracker:
  final case class Events(promptCompleted: Boolean, welcomeCompleted: Boolean)
