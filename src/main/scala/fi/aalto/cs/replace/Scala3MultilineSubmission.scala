package fi.aalto.cs.replace

import com.intellij.openapi.diagnostic.Logger

import java.io.OutputStream
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}
import scala.util.control.NonFatal

final class Scala3MultilineSubmission private (
    val sourceFile: Path,
    val replCommand: String
) extends AutoCloseable:

  def writeTo(outputStream: OutputStream): Unit =
    outputStream.write((replCommand + "\n").getBytes(UTF_8))
    outputStream.flush()

  private[replace] def delete(): Boolean =
    try
      Files.deleteIfExists(sourceFile)
      true
    catch
      case NonFatal(error) =>
        Scala3MultilineSubmission.logger.warn(
          s"Could not delete Scala 3 REPL submission file $sourceFile",
          error
        )
        false

  override def close(): Unit = delete()

object Scala3MultilineSubmission:

  private val logger = Logger.getInstance(classOf[Scala3MultilineSubmission])

  def create(source: String): Scala3MultilineSubmission =
    val sourceFile = Files.createTempFile("replace-repl-", ".scala").toAbsolutePath
    try
      Files.writeString(sourceFile, source, UTF_8)
      new Scala3MultilineSubmission(sourceFile, s":load $sourceFile")
    catch
      case error: Throwable =>
        try Files.deleteIfExists(sourceFile)
        catch case NonFatal(cleanupError) => error.addSuppressed(cleanupError)
        throw error
