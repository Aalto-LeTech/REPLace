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

/** Tracks which modules have been edited since their REPL was started, so open REPLs can warn that
  * they are running outdated code. Modules are tracked by name to avoid retaining [[Module]]
  * instances.
  */
@Service(Array(Service.Level.PROJECT))
final class ReplChangesService(project: Project) extends Disposable:
  private val modifiedModules   = ConcurrentHashMap.newKeySet[String]()
  private val activeReplCounts  = new ConcurrentHashMap[String, Integer]()
  private val listenerInstalled = new AtomicBoolean(false)

  /** Marks the module's code as applied, which a REPL start means. */
  def onReplStarted(module: Module): Unit =
    if listenerInstalled.compareAndSet(false, true) then
      EditorFactory.getInstance.getEventMulticaster.addDocumentListener(new ChangesListener(), this)
    activeReplCounts.merge(module.getName, 1, (count, _) => count + 1)
    modifiedModules.remove(module.getName)

  /** Drops one REPL from the module's open count. `computeIfPresent` removes the entry on null. */
  def onReplClosed(module: Module): Unit =
    activeReplCounts.computeIfPresent(
      module.getName,
      (_, count) => if count <= 1 then null else Integer.valueOf(count - 1)
    )

  def hasModuleChanged(module: Module): Boolean = modifiedModules.contains(module.getName)

  /** Only a parent disposable for the document listener; nothing of its own to release. */
  override def dispose(): Unit = ()

  private final class ChangesListener extends DocumentListener:
    override def documentChanged(event: DocumentEvent): Unit =
      if activeReplCounts.isEmpty || project.isDisposed || !project.isOpen then return
      Option(FileDocumentManager.getInstance.getFile(event.getDocument))
        .flatMap(file => Option(ProjectFileIndex.getInstance(project).getModuleForFile(file)))
        .map(_.getName)
        .filter(activeReplCounts.containsKey)
        .foreach(modifiedModules.add)

object ReplChangesService:
  def apply(project: Project): ReplChangesService = project.getService(classOf[ReplChangesService])
