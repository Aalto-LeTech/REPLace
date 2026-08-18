package fi.aalto.cs.replace

import com.intellij.execution.process.ProcessHandler

import java.io.{ByteArrayOutputStream, IOException, OutputStream}
import java.nio.charset.StandardCharsets.UTF_8
import java.util.Collections
import scala.jdk.CollectionConverters.*

/** Records everything written to the process input, grouped into one chunk per flush.
  *
  * Assertions should work on chunks rather than the raw stream where the IntelliJ console is
  * involved: the console additionally echoes text printed as USER_INPUT (from `addToHistory`) into
  * the process input asynchronously, and matching on chunks keeps tests deterministic regardless of
  * whether that echo has already arrived.
  */
final class RecordingProcessHandler extends ProcessHandler:
  private val chunks = Collections.synchronizedList(new java.util.ArrayList[String])
  var failWrites     = false

  private val failingProcessInput = new OutputStream:
    override def write(value: Int): Unit =
      throw new IOException("simulated write failure")

    override def write(bytes: Array[Byte], offset: Int, length: Int): Unit =
      throw new IOException("simulated write failure")

    override def flush(): Unit =
      throw new IOException("simulated write failure")

  private val recordingInput = new OutputStream:
    private val current = new ByteArrayOutputStream

    override def write(value: Int): Unit = current.synchronized(current.write(value))

    override def write(bytes: Array[Byte], offset: Int, length: Int): Unit =
      current.synchronized(current.write(bytes, offset, length))

    override def flush(): Unit = current.synchronized {
      if current.size() > 0 then
        chunks.add(current.toString(UTF_8))
        current.reset()
    }

  def stdinChunks: Seq[String] = chunks.synchronized(List.from(chunks.iterator().asScala))
  def stdinText: String        = stdinChunks.mkString
  def clearInput(): Unit       = chunks.clear()
  def terminate(): Unit        = notifyProcessTerminated(0)

  override def getProcessInput: OutputStream =
    if failWrites then failingProcessInput else recordingInput
  override protected def destroyProcessImpl(): Unit = notifyProcessTerminated(0)
  override protected def detachProcessImpl(): Unit  = notifyProcessDetached()
  override def detachIsDefault: Boolean             = false
