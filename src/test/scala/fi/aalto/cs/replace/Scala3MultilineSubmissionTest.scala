package fi.aalto.cs.replace

import dotty.tools.repl.ReplDriver
import org.junit.Assert.{assertEquals, assertFalse, assertTrue}
import org.junit.Test

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

  @Test def deletionFailureDoesNotEscapeCleanup(): Unit =
    val submission = Scala3MultilineSubmission.create("val answer = 42")
    val sourceFile = submission.sourceFile
    Files.delete(sourceFile)
    Files.createDirectory(sourceFile)
    val blockingFile = Files.writeString(sourceFile.resolve("still-in-use"), "test", UTF_8)

    try submission.close()
    finally
      Files.delete(blockingFile)
      Files.delete(sourceFile)

  @Test def loadsAnIndentationBasedBlockWithoutChangingBlankLines(): Unit =
    val submission = Scala3MultilineSubmission.create(multilineInput)
    val output     = new ByteArrayOutputStream()
    val driver     = new ReplDriver(Array("-usejavacp", "-color:never"), new PrintStream(output))

    try
      driver.run(submission.replCommand)(using driver.initialState)

      val replOutput = output.toString(UTF_8)
      assertTrue(replOutput, replOutput.contains("RESULT:3:alpha||omega"))
    finally submission.close()

  @Test def loadsThroughTheRealDumbTerminalRepl(): Unit =
    val submission = Scala3MultilineSubmission.create(multilineInput)

    try
      val replOutput = DumbTerminalRepl.run(Seq(submission.replCommand))
      assertTrue(replOutput, replOutput.contains(expectedResult))
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
      assertTrue(replOutput, replOutput.sliding("scala>".length).count(_ == "scala>") >= 4)
    finally
      firstSubmission.close()
      secondSubmission.close()

  private val expectedResult = "RESULT:3:alpha||omega"

  private def jetBrainsIssueCommands: Seq[String] = Seq(
    "class Foo:\n  def foo = 1\n",
    "  def bar = 2",
    """println(s"RESULT:${Foo().foo}:${Foo().bar}")"""
  )

  private def legacyCommands: Seq[String] =
    val tripleQuotes = "\"\"\""
    Seq(
      """|class Foo:
         |  def first = 1
         |  def second = 2
         |end Foo""".stripMargin,
      s"""val multiline = ${tripleQuotes}alpha
         |omega$tripleQuotes""".stripMargin,
      """println(s"RESULT:${Foo().first + Foo().second}:${multiline.linesIterator.mkString("|")}")"""
    )

  private def multilineInput: String =
    val tripleQuotes = "\"\"\""
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
