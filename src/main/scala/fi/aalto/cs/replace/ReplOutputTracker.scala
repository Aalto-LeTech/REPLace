package fi.aalto.cs.replace

/** Incrementally tracks REPL process output so that the prompt and the welcome line are recognized
  * however the console chunks them. A prompt counts only when the stream ends with the prompt token
  * at a line start, at the stream start, or directly after another prompt.
  */
private[replace] final class ReplOutputTracker(promptToken: String, welcomeLine: String):
  // Enough lookback for two prompt tokens and for the welcome line inside a longer chunk.
  private val maxKeptChars = math.max(512, promptToken.length + welcomeLine.length + 2)
  private var tail         = ""
  private var welcomeSeen  = false

  /** Appends a chunk and reports the stream events it completed. The empty-chunk guard is
    * load-bearing, or an empty print after a prompt would advance a paste twice.
    */
  def append(chunk: String): ReplOutputTracker.Events =
    if chunk.isEmpty then
      return ReplOutputTracker.Events(promptCompleted = false, welcomeCompleted = false)
    tail = (tail + chunk).takeRight(maxKeptChars)
    val welcomeCompleted = !welcomeSeen && tail.contains(welcomeLine)
    val events           = ReplOutputTracker.Events(endsAtPrompt, welcomeCompleted)
    // After the first prompt the welcome text can only come from evaluated code.
    welcomeSeen = welcomeSeen || welcomeCompleted || events.promptCompleted
    events

  def welcomePending: Boolean = !welcomeSeen

  private def endsAtPrompt: Boolean =
    tail.endsWith(promptToken) && {
      val before = tail.dropRight(promptToken.length)
      // The dumb-terminal REPL always writes "\n" before its prompt.
      before.isEmpty || before.endsWith("\n")
      // An empty submission makes the REPL print the next prompt with nothing in between.
      || before.endsWith(promptToken)
    }

private[replace] object ReplOutputTracker:
  final case class Events(promptCompleted: Boolean, welcomeCompleted: Boolean)
