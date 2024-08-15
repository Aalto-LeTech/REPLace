package fi.aalto.cs.replace.utils

import com.intellij.openapi.editor.EditorFactory
import com.intellij.openapi.editor.event.{DocumentEvent, DocumentListener}
import com.intellij.openapi.fileEditor.FileDocumentManager
import com.intellij.openapi.module.Module
import com.intellij.openapi.project.Project
import com.intellij.openapi.roots.ProjectFileIndex
import com.intellij.openapi.util.Disposer

import java.util
import java.util.Collections

object ReplChangesObserver:
  private var documentListenerInstalled = false
  private val disposable = Disposer.newDisposable
  private val modifiedModules = Collections.synchronizedSet(new util.HashSet[Module])

  /**
   * Triggered when a REPL has started for a particular module, indicating that all pending code changes
   * have been applied in the REPL as well.
   *
   * @param module The module for which the REPL is being opened.
   */
  def onStartedRepl(module: Module): Unit =
    if !documentListenerInstalled then
      EditorFactory.getInstance.getEventMulticaster.addDocumentListener(new ReplChangesObserver.ChangesListener(module.getProject), disposable)
      documentListenerInstalled = true
    modifiedModules.remove(module)

  /**
   * Triggered when a module undergoes some code change, which indicates that existing REPLs should
   * show a warning message that they're running an outdated version of the module.
   *
   * @param module The module which has been changed.
   */
  def onModuleChanged(module: Module): Unit =
    modifiedModules.add(module)

  def hasModuleChanged(module: Module): Boolean = modifiedModules.contains(module)

  private class ChangesListener(private val project: Project) extends DocumentListener:
    override def documentChanged(event: DocumentEvent): Unit =
      val file = FileDocumentManager.getInstance.getFile(event.getDocument)
      if file == null then return
      if !project.isDisposed && project.isOpen then
        val module = ProjectFileIndex.getInstance(project).getModuleForFile(file)
        if module != null then ReplChangesObserver.onModuleChanged(module)
  end ChangesListener
end ReplChangesObserver