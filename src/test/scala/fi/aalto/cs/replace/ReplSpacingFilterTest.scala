package fi.aalto.cs.replace

import org.junit.Assert.assertEquals
import org.junit.jupiter.api.Test

import scala.util.Random

/** Property-tests the spacing filter the way `Repl.print` drives it, with a real
  * [[ReplOutputTracker]] and a queue counter standing in for the paste state. Chunk boundaries are
  * arbitrary in the real console, so every chunking of one byte stream must render identical text.
  */
class ReplSpacingFilterTest:

  private val echoes = Seq("var a: Int = 1", "a: Int = 2", "a: Int = 3")

  /** What a replay printed to the "console" and what the filter still withheld at stream end, kept
    * separate so tests can tell hold-back from printing.
    */
  private final case class Replay(printed: String, leftover: String)

  /** Replays a REPL byte stream chunk by chunk, hiding prompts while groups remain queued. */
  private def replay(
      chunks: Seq[String],
      groupsQueued: Int,
      suppressBlank: Boolean = false
  ): Replay =
    val filter  = new ReplSpacingFilter("scala> ")
    val tracker = new ReplOutputTracker("scala> ", Repl.welcomeLine)
    val out     = new StringBuilder
    var queued  = groupsQueued
    filter.recordPrinted("scala> ")
    for chunk <- chunks do
      val normalized = filter.normalize(chunk)
      val prompt     = tracker.append(normalized).promptCompleted
      val sent       = prompt && queued > 0
      if sent then queued -= 1
      val rendered =
        filter.render(normalized, prompt, promptHidden = sent, suppressBlank)
      if rendered.body.nonEmpty then
        out ++= rendered.body
        filter.recordPrinted(rendered.body)
      if rendered.prompt.nonEmpty then
        out ++= rendered.prompt
        filter.recordPrinted(rendered.prompt)
    Replay(out.toString, filter.flush())

  /** The full rendered text; with `drained = true` nothing may remain withheld at stream end. */
  private def console(
      chunks: Seq[String],
      groupsQueued: Int,
      suppressBlank: Boolean = false,
      drained: Boolean = false
  ): String =
    val result = replay(chunks, groupsQueued, suppressBlank)
    if drained then
      assertEquals("nothing may be withheld once the paste has drained", "", result.leftover)
    result.printed + result.leftover

  /** Chunks never span two groups' echoes, because group n+1's `:load` is written only while
    * processing group n's prompt chunk.
    */
  private def randomChunks(stream: String, random: Random): Seq[String] =
    val cuts = (1 until stream.length).filter(_ => random.nextDouble() < 0.3)
    (Seq(0) ++ cuts ++ Seq(stream.length))
      .sliding(2)
      .map(window => stream.substring(window(0), window(1)))
      .toSeq

  @Test def randomBoundariesRenderTheSameText(): Unit =
    // Three spacing shapes: none beyond the terminator, the REPL's usual blank line, and a user
    // blank line that must survive the hidden prompt's single-newline consumption.
    for
      spacing <- Seq("\n", "\n\n", "\n\n\n")
      seed    <- Seq(20260828L, 20260831L)
    do
      val random = new Random(seed)
      // Every prompt, hidden or shown, consumes exactly one spacing newline. The echo's terminator
      // always survives, as do any user blank lines beyond the consumed one.
      val between  = "\n" * math.max(spacing.length - 1, 1)
      val expected = echoes.mkString(between) + between + "scala> "
      for _ <- 1 to 500 do
        val stream = echoes.flatMap(echo => randomChunks(s"$echo${spacing}scala> ", random))
        assertEquals(
          s"seed $seed, spacing ${spacing.length}, chunks ${stream.map(_.replace("\n", "\\n"))}",
          expected,
          console(stream, groupsQueued = 2, drained = true)
        )

  @Test def crLfSplitAcrossChunksRendersOneNewline(): Unit =
    // The CR is held for the next chunk, which completes the pair, so the console sees one newline
    // rather than a stray carriage return that returns the cursor and overwrites the line.
    assertEquals(
      "var a: Int = 1\nscala> ",
      console(Seq("var a: Int = 1\r", "\n\r\nscala> "), groupsQueued = 0, drained = true)
    )

  @Test def crLfWithinAChunkIsNormalized(): Unit =
    assertEquals(
      "var a: Int = 1\nscala> ",
      console(Seq("var a: Int = 1\r\n\r\nscala> "), groupsQueued = 0, drained = true)
    )

  @Test def promptLikeOutputIsHeldOnlyUntilTheNextChunk(): Unit =
    // Every prompt consumes the newline above it, so a chunk-final prompt prefix is always
    // ambiguous and is held back. The next chunk reclaims it, whatever it turns out to be.
    val held = replay(Seq("out\n", "scala"), groupsQueued = 0)
    assertEquals("out\n", held.printed)
    assertEquals("scala", held.leftover)
    val reclaimed = replay(Seq("out\n", "scala", "fish\n"), groupsQueued = 0)
    assertEquals("out\nscalafish\n", reclaimed.printed)
    assertEquals("", reclaimed.leftover)

  @Test def aLoneCarriageReturnIsPreserved(): Unit =
    assertEquals("a\rb", console(Seq("a\rb"), groupsQueued = 0))

  @Test def blankOutputIsHiddenWhileStartupCommandsAreFed(): Unit =
    // The spacing and prompts the REPL prints between startup commands must not reach the user.
    assertEquals(
      "",
      console(Seq("\n", "scala> "), groupsQueued = 0, suppressBlank = true, drained = true)
    )

  @Test def realOutputIsStillShownWhileStartupCommandsAreFed(): Unit =
    // Only whitespace is hidden while the commands are fed, because what a startup command prints
    // is the user's only sign that it ran.
    assertEquals(
      "Ready for o1\n",
      console(
        Seq("Ready for o1\n", "\n", "scala> "),
        groupsQueued = 0,
        suppressBlank = true,
        drained = true
      )
    )

  @Test def aChunkFinalCarriageReturnIsHeldForOneChunk(): Unit =
    // Only the next chunk tells a cursor return from half a CRLF pair, so the flush is what
    // releases it when no next chunk arrives, even while another paste group is queued.
    val result = replay(Seq("out\r"), groupsQueued = 1)
    assertEquals("out", result.printed)
    assertEquals("\r", result.leftover)

  @Test def aFlushAfterNormalizeKeepsThePendingCarriageReturn(): Unit =
    // Repl.print normalizes a chunk before it notices a content-type change, and the flush that
    // change triggers falls between the two. Releasing the carriage return there would print it
    // ahead of the very text it follows, so a "hello\n" chunk and a "world\r" chunk of another
    // type would reach the console as "hello\n" + "\r" + "world".
    val filter = new ReplSpacingFilter("scala> ")
    filter.recordPrinted("hello\n")
    val body = filter.normalize("world\r")

    assertEquals("", filter.flush(keepCarriageReturn = true))
    val rendered =
      filter.render(body, promptCompleted = false, promptHidden = false, suppressBlank = false)
    assertEquals("world", rendered.body)
    assertEquals("the carriage return waits for the next chunk", "\r", filter.flush())

  @Test def aCarriageReturnNotFollowedByANewlineIsEmittedIntact(): Unit =
    // A real cursor return is delayed by exactly one chunk, never dropped or split.
    assertEquals(
      "one\rtwo",
      console(Seq("one\r", "two"), groupsQueued = 0, drained = true)
    )

  @Test def consecutiveBareCarriageReturnsArePreserved(): Unit =
    assertEquals("one\rtwo\r", console(Seq("one\rtwo\r"), groupsQueued = 0))

  @Test def aVisiblePromptWhileDrainingConsumesItsSpacingNewline(): Unit =
    // The spurious prompt of a paste whose head is not sent yet is shown, and like every prompt it
    // consumes the spacing newline. Driven directly, because `replay` hides prompts while draining.
    val filter = new ReplSpacingFilter("scala> ")
    filter.recordPrinted("Loading\n")
    val rendered = filter.render(
      "\nscala> ",
      promptCompleted = true,
      promptHidden = false,
      suppressBlank = false
    )
    assertEquals("", rendered.body)
    assertEquals("scala> ", rendered.prompt)
    assertEquals("nothing may be withheld under a printed prompt", "", filter.flush())

  @Test def anUnterminatedLineBeforeAPromptIsTerminatedWithoutABlankLine(): Unit =
    // The single newline is that line's terminator, never the prompt's spacing.
    assertEquals(
      "res0: Int = 1\nscala> ",
      console(Seq("res0: Int = 1", "\nscala> "), groupsQueued = 0, drained = true)
    )

  @Test def aUserBlankLineBeforeAPromptSurvives(): Unit =
    // Terminator, the program's own blank line, and the prompt's spacing: only the last is eaten.
    assertEquals(
      "val age: Int = 20\n\nscala> ",
      console(Seq("val age: Int = 20\n\n\nscala> "), groupsQueued = 0, drained = true)
    )

  @Test def aSpacingNewlineInItsOwnChunkLeavesNoBlankLine(): Unit =
    // The usual shape: the output reader delivers the spacing newline as a complete line of its
    // own, one chunk before the prompt.
    assertEquals(
      "val age: Int = 20\nscala> ",
      console(Seq("val age: Int = 20\n", "\n", "scala> "), groupsQueued = 0, drained = true)
    )

  @Test def flushReturnsTextWithheldMidPaste(): Unit =
    // A partial prompt held back while groups are queued is reclaimed when the stream ends.
    val result = replay(Seq("var a: Int = 1\n\nscal"), groupsQueued = 1)
    assertEquals("var a: Int = 1\n", result.printed)
    assertEquals("\nscal", result.leftover)
