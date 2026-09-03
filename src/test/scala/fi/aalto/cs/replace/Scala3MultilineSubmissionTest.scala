package fi.aalto.cs.replace

import dotty.tools.repl.ReplDriver
import org.junit.Assert.{assertEquals, assertFalse, assertTrue}
import org.junit.jupiter.api.Test

import java.io.{ByteArrayOutputStream, PrintStream}
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.Files

class Scala3MultilineSubmissionTest:

  @Test def preservesTheSubmittedSourceExactly(): Unit =
    val input = """|class Esimerkki:
                   |  val greeting = "こんにちは"
                   |
                   |  val answer = 42
                   |end Esimerkki
                   |""".stripMargin.replace("\n", "\r\n")
    val submission = Scala3MultilineSubmission.create(input)

    try
      assertEquals(input, Files.readString(submission.sourceFile, UTF_8))
      assertEquals(s":load ${submission.sourceFile}", submission.replCommand)

      val processInput = new ByteArrayOutputStream()
      submission.writeTo(processInput)
      assertEquals(submission.replCommand + "\n", processInput.toString(UTF_8))
    finally submission.close()

    assertFalse(Files.exists(submission.sourceFile))

  @Test def padsSplitGroupsSoCompilerErrorsUseThePasteRelativeLineNumber(): Unit =
    val submission = Scala3MultilineSubmission.create("val answer = missing", lineOffset = 7)

    try
      assertEquals(
        "\n" * 7 + "val answer = missing",
        Files.readString(submission.sourceFile, UTF_8)
      )
      val replOutput =
        withDriver(driver => driver.run(submission.replCommand)(using driver.initialState))
      assertTrue(replOutput, replOutput.contains("\n8 |val answer = missing"))
    finally submission.close()

  @Test def deletionFailureDoesNotEscapeCleanup(): Unit =
    val submission = Scala3MultilineSubmission.create("val answer = 42")
    val sourceFile = submission.sourceFile
    Files.delete(sourceFile)
    Files.createDirectory(sourceFile)
    val blockingFile = Files.writeString(sourceFile.resolve("still-in-use"), "test", UTF_8)

    try
      submission.close()
      // Nothing escapes, and nothing that could not be deleted is lost either.
      assertTrue("close must leave the undeletable file alone", Files.exists(blockingFile))
    finally
      Files.delete(blockingFile)
      Files.delete(sourceFile)

  @Test def loadsAnIndentationBasedBlockWithoutChangingBlankLines(): Unit =
    val submission = Scala3MultilineSubmission.create(multilineInput)

    try
      val replOutput =
        withDriver(driver => driver.run(submission.replCommand)(using driver.initialState))
      assertTrue(replOutput, replOutput.contains(expectedResult))
    finally submission.close()

  /** The REPL compiles each submission as one wrapper object, in which every simple reassignment
    * gets a synthetic `<name>$assign` value, so two reassignments of one variable must be split or
    * the synthesized values collide. The hand-written chunks are pinned against the splitter by
    * [[Scala3StatementSplitterTest.testSplitsConsecutiveStatementsIntoTheirOwnGroups]].
    */
  @Test def loadsRepeatedReassignmentsOfTheSameVariableWhenSplitPerStatement(): Unit =
    val submissions = repeatedReassignmentChunks.map(Scala3MultilineSubmission.create(_))

    try
      val replOutput = withDriver { driver =>
        val loaded = submissions.foldLeft(driver.initialState) { (state, submission) =>
          driver.run(submission.replCommand)(using state)
        }
        driver.run("""println(s"RESULT:$a")""")(using loaded)
      }
      assertTrue(replOutput, replOutput.contains("RESULT:3"))
    finally submissions.foreach(_.close())

  /** The same paste as one whole-file `:load` is rejected outright, which is why splitting exists.
    */
  @Test def wholePasteLoadOfRepeatedReassignmentsStillCollides(): Unit =
    val submission = Scala3MultilineSubmission.create(repeatedReassignmentChunks.mkString("\n"))

    try
      val replOutput =
        withDriver(driver => driver.run(submission.replCommand)(using driver.initialState))
      assertTrue(replOutput, replOutput.contains("[E120]"))
    finally submission.close()

  @Test def legacyDirectSubmissionLosesBlankLinesInTheRealDumbTerminalRepl(): Unit =
    val replOutput = DumbTerminalRepl.run(legacyCommands)

    assertFalse(replOutput, replOutput.contains(expectedResult))
    assertTrue(replOutput, replOutput.contains("RESULT:3:alpha|omega"))

  @Test def directSubmissionPrematurelyEndsAnIndentationBlockAtAnEmptyLine(): Unit =
    val replOutput = DumbTerminalRepl.run(jetBrainsIssueCommands)

    assertFalse(replOutput, replOutput.contains("RESULT:1:2"))
    assertTrue(replOutput, replOutput.contains("// defined class Foo"))
    assertTrue(replOutput, replOutput.contains("def bar: Int"))
    assertTrue(replOutput, replOutput.contains("bar is not a member of Foo"))

  @Test def preservesStateAcrossMultiplePromptsAndMultilineSubmissions(): Unit =
    val firstSubmission = Scala3MultilineSubmission.create(
      """|class First:
         |  def first = 1
         |
         |  def second = 2
         |end First
         |""".stripMargin
    )
    val secondSubmission = Scala3MultilineSubmission.create(
      """|class Combined:
         |  def value = First().first + First().second
         |
         |  def label = "ready"
         |end Combined
         |""".stripMargin
    )

    try
      val commands = Seq(
        firstSubmission.replCommand,
        """println(s"PROMPT-1:${First().first}")""",
        secondSubmission.replCommand,
        """println(s"PROMPT-2:${Combined().value}:${Combined().label}")"""
      )
      val replOutput = DumbTerminalRepl.run(commands)

      assertTrue(replOutput, replOutput.contains("PROMPT-1:1"))
      assertTrue(replOutput, replOutput.contains("PROMPT-2:3:ready"))
      assertTrue(replOutput, DumbTerminalRepl.promptCount(replOutput) >= 4)
    finally
      firstSubmission.close()
      secondSubmission.close()

  @Test def anImportOnlyLoadPersistsIntoTheSession(): Unit =
    val importSubmission = Scala3MultilineSubmission.create("import scala.math.sqrt")
    try
      val replOutput = withDriver { driver =>
        val state = driver.run(importSubmission.replCommand)(using driver.initialState)
        driver.run("""println(s"RESULT:${sqrt(9.0)}")""")(using state)
      }
      assertTrue(replOutput, replOutput.contains("RESULT:3.0"))
    finally importSubmission.close()

  /** Extensions on a user type named like a primitive and on the primitive must share one
    * submission, or the second one hides the first in the REPL session.
    */
  @Test def keepsAUserTypeNamedIntAndScalaIntExtensionsAvailableTogether(): Unit =
    val submission = Scala3MultilineSubmission.create(
      "class Int\n" +
        "extension (value: Int) def tagged = \"CUSTOM\"\n" +
        "extension (value: scala.Int) def tagged = \"SCALA\""
    )
    try
      val replOutput = withDriver { driver =>
        val state = driver.run(submission.replCommand)(using driver.initialState)
        driver.run(
          """println(s"RESULT:${new Int().tagged}:${1.tagged}")"""
        )(using state)
      }
      assertTrue(replOutput, replOutput.contains("RESULT:CUSTOM:SCALA"))
    finally submission.close()

  /** Runs `body` against a fresh in-process REPL driver and returns everything it printed. Drivers
    * are not reusable, because a shared compiler collides on the REPL's wrapper-object indices.
    */
  private def withDriver(body: ReplDriver => Unit): String =
    val output = new ByteArrayOutputStream()
    body(new ReplDriver(Array("-usejavacp", "-color:never"), new PrintStream(output)))
    output.toString(UTF_8)

  private val repeatedReassignmentChunks = Seq("var a = 1", "a = a + 1", "a = a + 1")

  private val expectedResult = "RESULT:3:alpha||omega"

  private val tripleQuotes = "\"\"\""

  private val jetBrainsIssueCommands: Seq[String] = Seq(
    "class Foo:\n  def foo = 1\n",
    "  def bar = 2",
    """println(s"RESULT:${Foo().foo}:${Foo().bar}")"""
  )

  private val legacyCommands: Seq[String] = Seq(
    """|class Foo:
       |  def first = 1
       |  def second = 2
       |end Foo""".stripMargin,
    s"""val multiline = ${tripleQuotes}alpha
       |omega$tripleQuotes""".stripMargin,
    """println(s"RESULT:${Foo().first + Foo().second}:${multiline.linesIterator.mkString("|")}")"""
  )

  private val multilineInput: String =
    s"""|class Foo:
        |  def first = 1
        |
        |  def second = 2
        |end Foo
        |
        |val multiline = ${tripleQuotes}alpha
        |
        |omega$tripleQuotes
        |println(s"RESULT:$${Foo().first + Foo().second}:$${multiline.linesIterator.mkString("|")}")
        |""".stripMargin
