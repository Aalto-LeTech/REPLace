package fi.aalto.cs.replace

import org.jetbrains.plugins.scala.LatestScalaVersions
import org.jetbrains.plugins.scala.project.ScalaFeatures
import org.junit.Assert.assertEquals
import org.junit.jupiter.api.{Test, TestInstance}

/** The splitter turns a multiline paste into the submission groups that are each sent to the Scala
  * 3 REPL as one compilation unit. Statements that must share a unit stay together, and everything
  * else is its own group.
  *
  * These tests only read a `Project`, so the per-class test instance lets them share one.
  */
@TestInstance(TestInstance.Lifecycle.PER_CLASS)
class Scala3StatementSplitterTest extends ReplPlatformTestBase:

  // The light test module has no Scala SDK, so the module-derived feature set is not available
  // here; production passes Repl.parserFeatures (the module's own features) instead.
  private def split(source: String): List[String] =
    splitWithLineOffsets(source).map(_.source)

  private def splitWithLineOffsets(source: String): List[Scala3StatementSplitter.Chunk] =
    Scala3StatementSplitter.splitWithLineOffsets(
      source,
      ScalaFeatures.forParserTests(LatestScalaVersions.Scala_3_9),
      getProject
    )

  /** The whole source is one group: the split returns it trimmed, otherwise unchanged. */
  private def assertNotSplit(source: String): Unit =
    assertEquals(List(source.trim), split(source))

  /** No split was possible or needed, so the caller sends the raw source as a single `:load`. */
  private def assertNoChunks(source: String): Unit =
    assertEquals(Nil, split(source))

  @Test
  def testSplitsConsecutiveStatementsIntoTheirOwnGroups(): Unit =
    assertEquals(
      List("var a = 1", "a = a + 1", "a = a + 1"),
      split("var a = 1\na = a + 1\na = a + 1\n")
    )

  @Test
  def testKeepsAClassWithABlankLineInItsBodyIntact(): Unit =
    assertNotSplit("""|class Foo:
                      |  def foo = 1
                      |
                      |  def bar = 2
                      |""".stripMargin)

  @Test
  def testKeepsCompanionsAndConsecutiveDefsInOneGroup(): Unit =
    assertNotSplit("""|class Counter(val n: Int)
                      |object Counter:
                      |  def zero = Counter(0)
                      |def isEven(n: Int): Boolean = if n == 0 then true else isOdd(n - 1)
                      |def isOdd(n: Int): Boolean = if n == 0 then false else isEven(n - 1)
                      |""".stripMargin)

  @Test
  def testValsGetTheirOwnGroupsBetweenDefinitions(): Unit =
    assertEquals(
      List("def f(n: Int) = n + 1", "val x = f(1)", "val y = f(2)"),
      split("def f(n: Int) = n + 1\nval x = f(1)\nval y = f(2)\n")
    )

  @Test
  def testFunctionsSharingANameNeverShareAGroup(): Unit =
    // Plain functions are keyed by name alone, so a redefinition, an overload on a different
    // parameter type and a by-name/by-value pair (which cannot overload at all) all split.
    val pairs = List(
      "def g(n: Int) = 1" -> "def g(n: Int) = 2",
      "def g(n: Int) = 1" -> "def g(s: String) = 2",
      "def g(x: Int) = x" -> "def g(x: => Int) = 2"
    )
    pairs.foreach { (first, second) =>
      assertEquals(List(first, second), split(s"$first\n$second\n"))
    }

  @Test
  def testDoesNotSplitATripleQuotedStringWithABlankLine(): Unit =
    assertNotSplit("val multiline = \"\"\"alpha\n\nomega\"\"\"\n")

  @Test
  def testAbsorbsACommentIntoTheFollowingStatementsGroup(): Unit =
    assertEquals(
      List("// a comment\nval c = 1", "c + 1"),
      split("// a comment\nval c = 1\nc + 1\n")
    )

  @Test
  def testSplitsAPasteContainingAColonSyntaxGiven(): Unit =
    // `given X:` with an indented body is Scala 3.6+ syntax, and parsing must use features that
    // accept it, or every paste containing one falls back to a whole-paste `:load`.
    val given_ = "given Ord[Int]:\n  def compare(a: Int, b: Int) = a - b"
    assertEquals(
      List(given_, "var c = 0", "c = c + 1", "c = c + 1"),
      split(s"$given_\nvar c = 0\nc = c + 1\nc = c + 1\n")
    )

  @Test
  def testGivesEveryImportItsOwnGroup(): Unit =
    assertEquals(
      List(
        "def f(n: Int) = n.toDouble",
        "import scala.math.sqrt",
        "def g(n: Int) = sqrt(f(n))"
      ),
      split(
        "def f(n: Int) = n.toDouble\nimport scala.math.sqrt\ndef g(n: Int) = sqrt(f(n))\n"
      )
    )

  // Accepted line-by-line semantics, pinned so a refactor cannot flip them silently:

  @Test
  def testSplitsAForwardReferenceFromAValueToALaterDefinition(): Unit =
    // The val interrupts the run, so `f` is not yet defined when the val's group compiles, exactly
    // as if the lines were typed one at a time.
    assertEquals(
      List("val x = f(1)", "def f(n: Int) = n"),
      split("val x = f(1)\ndef f(n: Int) = n\n")
    )

  @Test
  def testSplitsOverloadsSeparatedByAValueIntoSeparateGroups(): Unit =
    // Line-by-line semantics: the second `f` replaces the first instead of overloading it.
    assertEquals(
      List("def f(n: Int) = 1", "val x = 0", "def f(s: String) = 2"),
      split("def f(n: Int) = 1\nval x = 0\ndef f(s: String) = 2\n")
    )

  @Test
  def testReturnsNoChunksForCommentOnlyInput(): Unit =
    assertNoChunks("// nothing but a comment\n")

  @Test
  def testSplitsARedefinedExtensionMethodIntoANewGroup(): Unit =
    // Two extensions defining the same method on the same type collide inside one unit exactly
    // like a repeated reassignment; typed line by line the second legally replaces the first.
    assertEquals(
      List("extension (n: Int) def twice = n * 2", "extension (n: Int) def twice = n * 3"),
      split("extension (n: Int) def twice = n * 2\nextension (n: Int) def twice = n * 3\n")
    )

  @Test
  def testKeepsDistinctExtensionMethodsInOneGroup(): Unit =
    assertNotSplit(
      "extension (n: Int) def twice = n * 2\nextension (s: String) def twice = s + s\n"
    )

  @Test
  def testSplitsAPlainFunctionFromACollidingExtension(): Unit =
    assertEquals(
      List("def twice(n: Int) = n * 2", "extension (n: Int) def twice = n * 3"),
      split("def twice(n: Int) = n * 2\nextension (n: Int) def twice = n * 3\n")
    )

  @Test
  def testKeepsExtensionsOnDifferentlySpelledReceiversTogether(): Unit =
    // Receivers are keyed by source text, so a second spelling of one type reads as a different
    // receiver and the two extensions share a unit.
    assertNotSplit(
      "extension (n: Int) def twice = n * 2\n" +
        "extension (n: scala.Int) def twice = n * 3\n"
    )

  @Test
  def testSplitsSemicolonSeparatedStatementsOnOneLine(): Unit =
    // All three groups came from the same line, so they all pad to line zero.
    assertEquals(
      List(
        Scala3StatementSplitter.Chunk("var a = 1", lineOffset = 0),
        Scala3StatementSplitter.Chunk("a = a + 1", lineOffset = 0),
        Scala3StatementSplitter.Chunk("a = a + 1", lineOffset = 0)
      ),
      splitWithLineOffsets("var a = 1; a = a + 1; a = a + 1\n")
    )

  @Test
  def testDoesNotSplitASemicolonInsideAStringLiteral(): Unit =
    assertNotSplit("val s = \"a;b\"\n")

  @Test
  def testATrailingSemicolonDoesNotBreakADefinitionRun(): Unit =
    assertNotSplit(
      "def isEven(n: Int): Boolean = n == 0 || isOdd(n - 1);\n" +
        "def isOdd(n: Int): Boolean = n != 0 && isEven(n - 1)\n"
    )

  @Test
  def testASoleTrailingSemicolonYieldsASingleChunk(): Unit =
    assertEquals(List("val a = 1"), split("val a = 1;\n"))

  @Test
  def testSplitsExtensionOverloadsOnTheSameReceiverByTheirSharedName(): Unit =
    assertEquals(
      List(
        "extension (n: Int) def plus(m: Int) = n + m",
        "extension (n: Int) def plus(m: String) = n.toString + m"
      ),
      split(
        "extension (n: Int) def plus(m: Int) = n + m\n" +
          "extension (n: Int) def plus(m: String) = n.toString + m\n"
      )
    )

  @Test
  def testSplitsAGivenAliasRedefiningAStructuralGiven(): Unit =
    // Both spellings bind the same term name, so they must not share a unit.
    val structural = "given ord: Ordering[Int]:\n  def compare(a: Int, b: Int) = a - b"
    assertEquals(
      List(structural, "given ord: Ordering[Int] = Ordering.Int"),
      split(s"$structural\ngiven ord: Ordering[Int] = Ordering.Int\n")
    )

  @Test
  def testSplitsAParameterizedNamedGivenFromAFunctionOfTheSameName(): Unit =
    val given_ = "given foo(using x: X): X = x"
    assertEquals(
      List(s"trait X\n$given_", "def foo(using x: X): X = x"),
      split(s"trait X\n$given_\ndef foo(using x: X): X = x\n")
    )

  @Test
  def testSplitsAParameterizedNamedStructuralGivenFromAFunctionOfTheSameName(): Unit =
    val given_ =
      "given foo(x: Int): Ordering[Int] with\n  def compare(a: Int, b: Int) = a - b"
    assertEquals(
      List(given_, "def foo(x: Int): Int = x"),
      split(s"$given_\ndef foo(x: Int): Int = x\n")
    )

  @Test
  def testKeepsDistinctAnonymousGivensTogether(): Unit =
    assertNotSplit(
      "trait A\n" +
        "trait B\n" +
        "given A = new A:\n  val b = summon[B]\n" +
        "given B = new B {}\n"
    )

  @Test
  def testSplitsRepeatedAnonymousGivensOfTheSameType(): Unit =
    assertEquals(
      List(
        "given Ordering[Int] = Ordering.Int",
        "given Ordering[Int] = Ordering.Int.reverse"
      ),
      split(
        "given Ordering[Int] = Ordering.Int\n" +
          "given Ordering[Int] = Ordering.Int.reverse\n"
      )
    )

  @Test
  def testSplitsRepeatedAnonymousStructuralGivensWithDifferentBodies(): Unit =
    val first =
      "given Ordering[Int] with\n  def compare(a: Int, b: Int) = a - b"
    val second =
      "given Ordering[Int] with\n  def compare(a: Int, b: Int) = b - a"
    assertEquals(List(first, second), split(s"$first\n$second\n"))

  @Test
  def testEquivalentAnonymousGivenTypeSpellingsCollide(): Unit =
    assertEquals(
      List(
        "given Ordering[Int] = Ordering.Int",
        "given scala.math.Ordering[scala.Int] = Ordering.Int.reverse"
      ),
      split(
        "given Ordering[Int] = Ordering.Int\n" +
          "given scala.math.Ordering[scala.Int] = Ordering.Int.reverse\n"
      )
    )

  @Test
  def testAnonymousGivenTypesWithTheSameGeneratedNameCollide(): Unit =
    assertEquals(
      List(
        "given Ordering[List[Int]] = Ordering.by(_.sum)",
        "given Ordering[List[String]] = Ordering.by(_.mkString)"
      ),
      split(
        "given Ordering[List[Int]] = Ordering.by(_.sum)\n" +
          "given Ordering[List[String]] = Ordering.by(_.mkString)\n"
      )
    )

  @Test
  def testAnonymousGivenCollidesWithAnExplicitUseOfItsGeneratedName(): Unit =
    assertEquals(
      List("given Ordering[Int] = Ordering.Int", "def given_Ordering_Int = 1"),
      split("given Ordering[Int] = Ordering.Int\ndef given_Ordering_Int = 1\n")
    )

  @Test
  def testKeepsAnObjectWithAParameterizedFunctionOfTheSameName(): Unit =
    assertNotSplit("object OQ:\n  val n = OQ(1)\ndef OQ(x: Int) = x\n")

  @Test
  def testSplitsSequentialImportsThatBindTheSameName(): Unit =
    val definitions = "object A:\n  def x = 1\nobject B:\n  def x = 2"
    // Named and wildcard imports alike: each one is its own group, so a later import shadows an
    // earlier one exactly as it would line by line.
    List("x", "*").foreach { selector =>
      assertEquals(
        List(definitions, s"import A.$selector", s"import B.$selector", "println(x)"),
        split(s"$definitions\nimport A.$selector\nimport B.$selector\nprintln(x)\n")
      )
    }

  @Test
  def testGivesAnExportItsOwnGroup(): Unit =
    assertEquals(
      List("object A:\n  def x = 1", "export A.x", "def y = x + 1"),
      split("object A:\n  def x = 1\nexport A.x\ndef y = x + 1\n")
    )

  @Test
  def testSplitsACaseClassFromAParameterlessFunctionOfTheSameName(): Unit =
    assertEquals(
      List("case class CY(x: Int)", "def CY = 1"),
      split("case class CY(x: Int)\ndef CY = 1\n")
    )

  @Test
  def testSplitsAnEnumFromAParameterlessFunctionOfTheSameName(): Unit =
    assertEquals(
      List("enum Color:\n  case Red", "def Color = 1"),
      split("enum Color:\n  case Red\ndef Color = 1\n")
    )

  @Test
  def testKeepsExplicitCompanionsForCaseClassesAndEnums(): Unit =
    assertNotSplit(
      "case class Point(x: Int)\n" +
        "object Point:\n  def origin = Point(0)\n" +
        "enum Color:\n  case Red\n" +
        "object Color:\n  def default = Color.Red\n"
    )

  @Test
  def testPreservesIndentationOfEveryStatementInAGroup(): Unit =
    // The REPL accepts a uniformly indented unit, but not one whose first statement was dedented
    // to column zero while the rest kept their original columns.
    val source = "  def f = 1\n  def g = 2\n  var a = 0\n"
    assertEquals(List("  def f = 1\n  def g = 2", "  var a = 0"), split(source))

  @Test
  def testLeadingBlankLinesDoNotShiftChunkOffsets(): Unit =
    // The PSI factory must parse the untrimmed text: parsing a trimmed copy shifts every element
    // offset against the original and corrupts each reconstructed chunk.
    assertEquals(
      List("var a = 1", "a = a + 1"),
      split("\n\nvar a = 1\na = a + 1\n")
    )

  @Test
  def testRetainsPasteRelativeLineOffsetsForEveryGroup(): Unit =
    assertEquals(
      List(
        Scala3StatementSplitter.Chunk("var a = 1", lineOffset = 2),
        Scala3StatementSplitter.Chunk("a =\n  a + 1", lineOffset = 4),
        Scala3StatementSplitter.Chunk("println(a)", lineOffset = 6)
      ),
      splitWithLineOffsets("\n\nvar a = 1\n\na =\n  a + 1\nprintln(a)\n")
    )

  @Test
  def testReturnsNoChunksWhenTheSourceDoesNotParse(): Unit =
    assertNoChunks("val x = (1 +\ndef\n")

  @Test
  def testReturnsNoChunksForAnUnterminatedBlockComment(): Unit =
    assertNoChunks("val x = 1\nval y = 2\n/* unterminated")

  @Test
  def testReturnsNoChunksWhenOnlyTheInnerNestedBlockCommentClosesAtEof(): Unit =
    assertNoChunks("val x = 1\n/* outer /* inner */")

  @Test
  def testNormalizesCarriageReturnsAndCountsTheirLines(): Unit =
    // Both a lone CR and a CRLF pair count as exactly one line, so neither shifts the offsets a
    // pasted source's chunks are padded to.
    List("var a = 1\ra = missing\r", "var a = 1\r\na = missing\r\n").foreach { source =>
      assertEquals(
        // Escaped, or the carriage returns would overwrite the failure message in a terminal.
        source.replace("\r", "\\r"),
        List(
          Scala3StatementSplitter.Chunk("var a = 1", lineOffset = 0),
          Scala3StatementSplitter.Chunk("a = missing", lineOffset = 1)
        ),
        splitWithLineOffsets(source)
      )
    }

  @Test
  def testReturnsNoChunksForBlankInput(): Unit =
    assertNoChunks("\n\n")

  @Test
  def testReconstructsStatementTextExactlyFromTheOriginal(): Unit =
    val statement = "val s =\n  \"indented\" + // trailing comment\n    \"tail\""
    assertEquals(List(statement, "println(s)"), split(s"$statement\nprintln(s)\n"))
