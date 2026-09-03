package fi.aalto.cs.replace

import com.intellij.execution.process.ProcessHandler

import java.io.{ByteArrayOutputStream, IOException, OutputStream}
import java.nio.charset.StandardCharsets.UTF_8
import java.util.Collections
import scala.jdk.CollectionConverters.*

/** Records everything written to the process input, grouped into one chunk per flush.
  *
  * Assert on chunks rather than the raw stream wherever the IntelliJ console is involved, because
  * the console also echoes USER_INPUT text into the process input asynchronously, and chunk
  * matching stays deterministic whether or not that echo has arrived.
  */
final class RecordingProcessHandler extends ProcessHandler:
  private val chunks   = Collections.synchronizedList(new java.util.ArrayList[String])
  var failWrites       = false
  var nullProcessInput = false

  /** Runs before the fake pipe accepts a write, to model a process that dies with the bytes in
    * flight. Key hooks on the payload, or the asynchronous USER_INPUT echo can consume a one-shot.
    */
  var onWrite: String => Unit = _ => ()

  /** Runs after the bytes are accepted but before write returns, to model a fast child process. */
  var afterBytesAccepted: String => Unit = _ => ()

  private val failingProcessInput = new OutputStream:
    override def write(value: Int): Unit =
      throw new IOException("simulated write failure")

  private val recordingInput = new OutputStream:
    private val current = new ByteArrayOutputStream

    override def write(value: Int): Unit =
      val text = value.toChar.toString
      onWrite(text)
      if isProcessTerminated then
        throw new IOException("process terminated before bytes were accepted")
      current.synchronized(current.write(value))
      afterBytesAccepted(text)

    override def write(bytes: Array[Byte], offset: Int, length: Int): Unit =
      val text = new String(bytes, offset, length, UTF_8)
      onWrite(text)
      if isProcessTerminated then
        throw new IOException("process terminated before bytes were accepted")
      current.synchronized(current.write(bytes, offset, length))
      afterBytesAccepted(text)

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
    if nullProcessInput then null
    else if failWrites then failingProcessInput
    else recordingInput
  override protected def destroyProcessImpl(): Unit = notifyProcessTerminated(0)
  override protected def detachProcessImpl(): Unit  = notifyProcessDetached()
  override def detachIsDefault: Boolean             = false
