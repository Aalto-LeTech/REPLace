package fi.aalto.cs.replace

import org.junit.Assert.{assertFalse, assertTrue}
import org.junit.jupiter.api.Test

class ReplOutputTrackerTest:
  private val welcome = "Type in expressions for evaluation. Or try :help.\n"

  private def tracker = new ReplOutputTracker("scala> ", welcome)

  @Test def promptAtStreamStartCompletes(): Unit =
    assertTrue(tracker.append("scala> ").promptCompleted)

  @Test def promptAfterNewlineCompletes(): Unit =
    val t = tracker
    assertFalse(t.append("val res0: Int = 3\n").promptCompleted)
    assertTrue(t.append("scala> ").promptCompleted)
    // The same output and prompt arriving as one chunk complete it just as well.
    assertTrue(tracker.append("val res0: Int = 3\nscala> ").promptCompleted)

  @Test def promptSplitAcrossChunksCompletes(): Unit =
    val t = tracker
    assertFalse(t.append("val res0: Int = 3\nscala>").promptCompleted)
    assertTrue(t.append(" ").promptCompleted)

  @Test def promptWithoutTrailingSpaceDoesNotComplete(): Unit =
    assertFalse(tracker.append("scala>").promptCompleted)

  @Test def promptInTheMiddleOfALineDoesNotComplete(): Unit =
    assertFalse(tracker.append("printed scala> ").promptCompleted)

  @Test def completeOutputLineEqualToThePromptDoesNotComplete(): Unit =
    assertFalse(tracker.append("scala> \n").promptCompleted)

  @Test def promptDirectlyAfterAnotherPromptTokenDoesNotComplete(): Unit =
    val t = tracker
    assertFalse(t.append("scala>").promptCompleted)
    assertFalse(t.append("scala> ").promptCompleted)

  @Test def promptImmediatelyAfterAnotherPromptCompletes(): Unit =
    // A user submitting an empty line makes the REPL print a new prompt with no
    // output (not even a newline) in between.
    val t = tracker
    assertTrue(t.append("scala> ").promptCompleted)
    assertTrue(t.append("scala> ").promptCompleted)

  @Test def chunkAfterAPromptDoesNotReportThePromptAgain(): Unit =
    val t = tracker
    assertTrue(t.append("scala> ").promptCompleted)
    assertFalse(t.append("output").promptCompleted)

  @Test def carriageReturnLineEndingPrecedingPromptCompletes(): Unit =
    val t = tracker
    assertFalse(t.append("val res0: Int = 3\r\n").promptCompleted)
    assertTrue(t.append("scala> ").promptCompleted)

  @Test def emptyChunkReportsNothing(): Unit =
    val t = tracker
    t.append("scala> ")
    val events = t.append("")
    assertFalse(events.promptCompleted)
    assertFalse(events.welcomeCompleted)

  @Test def welcomeLineInOneChunkCompletes(): Unit =
    assertTrue(tracker.append(welcome).welcomeCompleted)

  @Test def welcomeLineSplitAcrossChunksCompletes(): Unit =
    val t = tracker
    assertFalse(t.append("Type in expressions for eval").welcomeCompleted)
    assertTrue(t.append("uation. Or try :help.\n").welcomeCompleted)

  @Test def welcomeLineIsReportedOnlyOnce(): Unit =
    val t = tracker
    assertTrue(t.append(welcome).welcomeCompleted)
    assertFalse(t.append(welcome).welcomeCompleted)

  @Test def welcomeLinePrintedAfterTheFirstPromptIsNotTheBanner(): Unit =
    // The REPL's banner precedes its first prompt; the same line later is output from user code.
    val t = tracker
    assertTrue(t.append("scala> ").promptCompleted)
    assertFalse(t.append(welcome).welcomeCompleted)

  @Test def promptStaysDetectableAfterLongOutput(): Unit =
    val t = tracker
    t.append("x" * 10000 + "\n")
    assertTrue(t.append("scala> ").promptCompleted)
