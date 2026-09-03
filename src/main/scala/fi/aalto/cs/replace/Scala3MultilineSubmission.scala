package fi.aalto.cs.replace

import com.intellij.openapi.diagnostic.Logger
import org.intellij.lang.annotations.Language

import java.io.OutputStream
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}
import scala.util.control.NonFatal

final class Scala3MultilineSubmission private (val sourceFile: Path) extends AutoCloseable:

  val replCommand: String = s":load $sourceFile"

  def writeTo(outputStream: OutputStream): Unit =
    outputStream.write((replCommand + "\n").getBytes(UTF_8))
    outputStream.flush()

  override def close(): Unit =
    try Files.deleteIfExists(sourceFile)
    catch
      case NonFatal(error) =>
        // Keep deleteOnExit as a fallback only for a file the OS still holds locked. Registering
        // every successful submission would retain all of their paths until the IDE exits.
        Scala3MultilineSubmission.scheduleDeletion(sourceFile, error)
        Scala3MultilineSubmission.logger.warn(
          s"Could not delete Scala 3 REPL submission file $sourceFile",
          error
        )

object Scala3MultilineSubmission:

  private val logger = Logger.getInstance(classOf[Scala3MultilineSubmission])

  private def scheduleDeletion(path: Path, originalError: Throwable): Unit =
    try path.toFile.deleteOnExit()
    catch case NonFatal(scheduleError) => originalError.addSuppressed(scheduleError)

  def create(
      @Language("Scala") source: String,
      lineOffset: Int = 0
  ): Scala3MultilineSubmission =
    require(lineOffset >= 0, "line offset must not be negative")
    val sourceFile = Files.createTempFile("replace-repl-", ".scala").toAbsolutePath
    try
      // Keep compiler diagnostics anchored to the line where this split group appeared in the
      // original paste. Blank leading lines are semantically inert in a :load source file.
      Files.writeString(sourceFile, "\n" * lineOffset + source, UTF_8)
      new Scala3MultilineSubmission(sourceFile)
    catch
      case error: Throwable =>
        try Files.deleteIfExists(sourceFile)
        catch
          case NonFatal(cleanupError) =>
            scheduleDeletion(sourceFile, cleanupError)
            error.addSuppressed(cleanupError)
        throw error
