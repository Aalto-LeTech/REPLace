package fi.aalto.cs.replace

import org.junit.Assert.{assertEquals, assertNotNull, assertTrue}

import java.io.ByteArrayOutputStream
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.Path
import java.util.concurrent.TimeUnit

/** Runs a real Scala REPL as an external process in a dumb terminal, feeding it one command per
  * prompt. That is the same pacing the plugin uses against a live REPL.
  */
object DumbTerminalRepl:

  /** Feeds the commands to the REPL, waiting for a prompt before each one, and returns the complete
    * transcript.
    */
  def run(commands: Seq[String]): String =
    val replClasspath = System.getProperty("replace.scalaReplTestClasspath")
    assertNotNull("replace.scalaReplTestClasspath is not set (run through Gradle)", replClasspath)
    val javaExecutable = Path.of(
      System.getProperty("java.home"),
      "bin",
      if System.getProperty("os.name").startsWith("Windows") then "java.exe" else "java"
    )
    val processBuilder = new ProcessBuilder(
      javaExecutable.toString,
      "-cp",
      replClasspath,
      "dotty.tools.repl.Main",
      "-usejavacp",
      "-color:never"
    ).redirectErrorStream(true)
    processBuilder.environment().put("TERM", "dumb")
    val process = processBuilder.start()
    // The transcript buffer doubles as the reader/waiter monitor.
    val output = new ByteArrayOutputStream
    val outputReader = new Thread(
      () =>
        val buffer    = new Array[Byte](1024)
        var bytesRead = process.getInputStream.read(buffer)
        while bytesRead >= 0 do
          output.synchronized {
            output.write(buffer, 0, bytesRead)
            output.notifyAll()
          }
          bytesRead = process.getInputStream.read(buffer)
      ,
      "scala-repl-test-output"
    )
    outputReader.start()

    // The whole transcript is rescanned on each wake so a prompt split across two reads still
    // counts; the deadline turns a promptless REPL into a test failure instead of a hung build.
    def awaitPrompt(expectedCount: Int): Unit =
      val deadline = System.nanoTime() + TimeUnit.SECONDS.toNanos(20)
      output.synchronized {
        var transcript = output.toString(UTF_8)
        while process.isAlive && promptCount(transcript) < expectedCount do
          val remainingNanos = deadline - System.nanoTime()
          assertTrue(transcript, remainingNanos > 0)
          output.wait(Math.max(1, TimeUnit.NANOSECONDS.toMillis(remainingNanos)))
          transcript = output.toString(UTF_8)
        assertTrue(transcript, promptCount(transcript) >= expectedCount)
      }

    try
      val processInput = process.getOutputStream
      awaitPrompt(1)
      commands.zipWithIndex.foreach { (command, index) =>
        processInput.write((command + "\n").getBytes(UTF_8))
        processInput.flush()
        awaitPrompt(index + 2)
      }
      processInput.write(":quit\n".getBytes(UTF_8))
      processInput.flush()
      processInput.close()

      assertTrue("Scala REPL did not exit", process.waitFor(20, TimeUnit.SECONDS))
      outputReader.join(TimeUnit.SECONDS.toMillis(5))
      val replOutput = output.synchronized(output.toString(UTF_8))
      assertEquals(replOutput, 0, process.exitValue())
      replOutput
    finally process.destroyForcibly()

  def promptCount(output: String): Int =
    output.sliding("scala>".length).count(_ == "scala>")
