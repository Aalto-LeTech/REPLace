package fi.aalto.cs.replace

import com.intellij.execution.ui.ConsoleViewContentType
import com.intellij.openapi.util.Disposer
import com.intellij.testFramework.fixtures.BasePlatformTestCase
import org.junit.Assert.{assertEquals, assertFalse, assertTrue}

class ReplStartupCommandsTest extends BasePlatformTestCase:

  def testWaitsForAReplPromptBeforeEachStartupCommand(): Unit =
    val process = new RecordingProcessHandler
    val repl = new Repl(getModule):
      override private[replace] def initialCommands: List[String] =
        List("import o1.*", "import o1.goodstuff.*", "import o1.goodstuff.gui.*")
      override private[replace] def scala3Module: Boolean = true
    repl.attachToProcess(process)

    try
      assertFalse(repl.isReadyForUserInput)

      repl.print(
        "Type in expressions for evaluation. Or try :help.\n",
        ConsoleViewContentType.NORMAL_OUTPUT
      )
      assertEquals("", process.stdinText)
      assertFalse(repl.isReadyForUserInput)

      repl.print("scala> ", ConsoleViewContentType.NORMAL_OUTPUT)
      assertEquals("import o1.*\n", process.stdinText)
      assertFalse(repl.isReadyForUserInput)

      repl.print("scala> ", ConsoleViewContentType.NORMAL_OUTPUT)
      assertEquals("import o1.*\nimport o1.goodstuff.*\n", process.stdinText)
      assertFalse(repl.isReadyForUserInput)

      repl.print("scala> ", ConsoleViewContentType.NORMAL_OUTPUT)
      assertEquals(
        "import o1.*\nimport o1.goodstuff.*\nimport o1.goodstuff.gui.*\n",
        process.stdinText
      )
      assertFalse(repl.isReadyForUserInput)

      repl.print("scala> ", ConsoleViewContentType.NORMAL_OUTPUT)
      assertTrue(repl.isReadyForUserInput)
    finally Disposer.dispose(repl)

  def testSendsStartupCommandsWhenPromptsArriveSplitAcrossChunks(): Unit =
    val process = new RecordingProcessHandler
    val repl = new Repl(getModule):
      override private[replace] def initialCommands: List[String] =
        List("import o1.*", "import o1.goodstuff.*")
      override private[replace] def scala3Module: Boolean = true
    repl.attachToProcess(process)

    try
      repl.print(
        "Type in expressions for evaluation. Or try :help.\n",
        ConsoleViewContentType.NORMAL_OUTPUT
      )
      repl.print("scala>", ConsoleViewContentType.NORMAL_OUTPUT)
      assertEquals("", process.stdinText)

      repl.print(" ", ConsoleViewContentType.NORMAL_OUTPUT)
      assertEquals("import o1.*\n", process.stdinText)

      repl.print("import o1.*\nscala>", ConsoleViewContentType.NORMAL_OUTPUT)
      repl.print(" ", ConsoleViewContentType.NORMAL_OUTPUT)
      assertEquals("import o1.*\nimport o1.goodstuff.*\n", process.stdinText)
      assertFalse(repl.isReadyForUserInput)

      repl.print("import o1.goodstuff.*\nscala> ", ConsoleViewContentType.NORMAL_OUTPUT)
      assertTrue(repl.isReadyForUserInput)
    finally Disposer.dispose(repl)
