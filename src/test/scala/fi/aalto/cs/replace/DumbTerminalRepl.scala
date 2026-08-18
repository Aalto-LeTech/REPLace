package fi.aalto.cs.replace

import org.junit.Assert.{assertEquals, assertTrue}

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
    val javaExecutable = Path.of(
      System.getProperty("java.home"),
      "bin",
      if System.getProperty("os.name").startsWith("Windows") then "java.exe" else "java"
    )
    val processBuilder = new ProcessBuilder(
      javaExecutable.toString,
      "-cp",
      System.getProperty("replace.scalaReplTestClasspath"),
      "dotty.tools.repl.Main",
      "-usejavacp",
      "-color:never"
    ).redirectErrorStream(true)
    processBuilder.environment().put("TERM", "dumb")
    val process    = processBuilder.start()
    val output     = new ByteArrayOutputStream
    val outputLock = new Object
    val outputReader = new Thread(
      () =>
        val buffer    = new Array[Byte](1024)
        var bytesRead = process.getInputStream.read(buffer)
        while bytesRead >= 0 do
          outputLock.synchronized {
            output.write(buffer, 0, bytesRead)
            outputLock.notifyAll()
          }
          bytesRead = process.getInputStream.read(buffer)
      ,
      "scala-repl-test-output"
    )
    outputReader.start()

    try
      val processInput = process.getOutputStream
      awaitPrompt(process, output, outputLock, expectedCount = 1)
      commands.zipWithIndex.foreach { (command, index) =>
        processInput.write((command + "\n").getBytes(UTF_8))
        processInput.flush()
        awaitPrompt(process, output, outputLock, expectedCount = index + 2)
      }
      processInput.write(":quit\n".getBytes(UTF_8))
      processInput.flush()
      processInput.close()

      assertTrue("Scala REPL did not exit", process.waitFor(20, TimeUnit.SECONDS))
      outputReader.join(TimeUnit.SECONDS.toMillis(5))
      val replOutput = outputLock.synchronized(output.toString(UTF_8))
      assertEquals(replOutput, 0, process.exitValue())
      replOutput
    finally process.destroyForcibly()

  def promptCount(output: String): Int =
    output.sliding("scala>".length).count(_ == "scala>")

  private def awaitPrompt(
      process: Process,
      output: ByteArrayOutputStream,
      outputLock: Object,
      expectedCount: Int
  ): Unit =
    val deadline = System.nanoTime() + TimeUnit.SECONDS.toNanos(20)
    outputLock.synchronized {
      while process.isAlive && promptCount(output.toString(UTF_8)) < expectedCount do
        val remainingNanos = deadline - System.nanoTime()
        assertTrue(output.toString(UTF_8), remainingNanos > 0)
        outputLock.wait(Math.max(1, TimeUnit.NANOSECONDS.toMillis(remainingNanos)))
      val replOutput = output.toString(UTF_8)
      assertTrue(replOutput, promptCount(replOutput) >= expectedCount)
    }
