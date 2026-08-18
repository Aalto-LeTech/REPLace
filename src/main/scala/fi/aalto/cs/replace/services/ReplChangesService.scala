package fi.aalto.cs.replace.services

import com.intellij.openapi.Disposable
import com.intellij.openapi.components.Service
import com.intellij.openapi.editor.EditorFactory
import com.intellij.openapi.editor.event.{DocumentEvent, DocumentListener}
import com.intellij.openapi.fileEditor.FileDocumentManager
import com.intellij.openapi.module.Module
import com.intellij.openapi.project.Project
import com.intellij.openapi.roots.ProjectFileIndex

import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicBoolean
import scala.annotation.tailrec

/** Tracks which modules of this project have been edited since their REPL was started, so open
  * REPLs can warn that they are running outdated code.
  *
  * Modules are tracked by name to avoid retaining [[Module]] instances, and document changes are
  * only inspected while at least one REPL is open for the project.
  */
@Service(Array(Service.Level.PROJECT))
final class ReplChangesService(project: Project) extends Disposable:
  private val modifiedModules   = ConcurrentHashMap.newKeySet[String]()
  private val activeReplCounts  = new ConcurrentHashMap[String, Integer]()
  private val listenerInstalled = new AtomicBoolean(false)

  /** Triggered when a REPL has started for a particular module, indicating that all pending code
    * changes have been applied in the REPL as well.
    */
  def onReplStarted(module: Module): Unit =
    if listenerInstalled.compareAndSet(false, true) then
      EditorFactory.getInstance.getEventMulticaster.addDocumentListener(ChangesListener(), this)
    activeReplCounts.merge(module.getName, 1, (count, one) => count + one)
    modifiedModules.remove(module.getName)

  /** Drops one REPL from the module's open count, removing the entry entirely when the last one
    * closes. Written as a compare-and-set retry rather than `computeIfPresent`, whose "remove this
    * key" signal is a `null` return from the remapping function.
    */
  @tailrec
  def onReplClosed(module: Module): Unit =
    val name = module.getName
    Option(activeReplCounts.get(name)) match
      case None => ()
      case Some(count) =>
        val updated =
          if count <= 1 then activeReplCounts.remove(name, count)
          else activeReplCounts.replace(name, count, count - 1)
        if !updated then onReplClosed(module)

  def hasModuleChanged(module: Module): Boolean = modifiedModules.contains(module.getName)

  override def dispose(): Unit = ()

  private final class ChangesListener extends DocumentListener:
    override def documentChanged(event: DocumentEvent): Unit =
      if activeReplCounts.isEmpty || project.isDisposed || !project.isOpen then return
      val changedModule = Option(FileDocumentManager.getInstance.getFile(event.getDocument))
        .flatMap(file => Option(ProjectFileIndex.getInstance(project).getModuleForFile(file)))
        .map(_.getName)
        .filter(activeReplCounts.containsKey)
      changedModule.foreach(modifiedModules.add)

object ReplChangesService:
  def apply(project: Project): ReplChangesService = project.getService(classOf[ReplChangesService])
