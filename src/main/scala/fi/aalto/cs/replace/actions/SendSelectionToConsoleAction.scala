package fi.aalto.cs.replace.actions

import com.intellij.openapi.actionSystem.{AnActionEvent, CommonDataKeys}
import com.intellij.openapi.actionSystem.ex.ActionUtil
import com.intellij.openapi.actionSystem.impl.SimpleDataContext
import com.intellij.openapi.editor.EditorFactory
import com.intellij.openapi.project.Project
import fi.aalto.cs.replace.Repl
import fi.aalto.cs.replace.utils.ModuleUtils
import org.jetbrains.plugins.scala.console.ScalaConsoleInfo
import org.jetbrains.plugins.scala.console.replace.ConsoleExecuteAction

/** Routes editor selections through REPLace's existing console action, so multiline selections get
  * the same pacing and readiness checks as pasted console input. Other Scala consoles keep the
  * stock action.
  */
class SendSelectionToConsoleAction
    extends org.jetbrains.plugins.scala.console.actions.SendSelectionToConsoleAction:

  override def actionPerformed(event: AnActionEvent): Unit =
    Option(event.getData(CommonDataKeys.PROJECT))
      .flatMap(project =>
        replOfTheSelectedModule(event, project)
          .orElse(Option(ScalaConsoleInfo.getConsole(project)))
      ) match
      case Some(repl: Repl) =>
        // The console may not have had focus since the last edit, so its banner can be stale.
        repl.refreshChangeBanner()
        if !repl.isReadyForUserInput then repl.showInputUnavailableHint()
        else
          Option(event.getData(CommonDataKeys.EDITOR))
            .flatMap(editor => Option(editor.getSelectionModel.getSelectedText)) match
            case Some(text) =>
              repl.setInputText(text)
              val consoleContext = SimpleDataContext.getSimpleContext(
                CommonDataKeys.EDITOR,
                repl.getConsoleEditor,
                event.getDataContext
              )
              ActionUtil.performAction(
                new ConsoleExecuteAction(),
                event.withDataContext(consoleContext)
              )
            // Nothing to send, and the stock action throws on a null selection.
            case None => ()
      case _ => super.actionPerformed(event)

  /** A console started for the module the selection comes from. `ScalaConsoleInfo` answers per
    * project with the console started last, and REPLace starts one console per module.
    */
  private def replOfTheSelectedModule(event: AnActionEvent, project: Project): Option[Repl] =
    ModuleUtils
      .moduleFromContext(project, event.getDataContext, _ => true)
      .flatMap { module =>
        // A stopped console outlives its process in a pinned run tab, so a usable one comes first.
        val consoles = projectRepls(project).filter(_.module == module)
        consoles.find(_.isReadyForUserInput).orElse(consoles.headOption)
      }

  /** Every REPLace console of the project, found through its editors, because the Scala plugin's
    * registry is queryable by editor but not enumerable.
    */
  private def projectRepls(project: Project): Seq[Repl] =
    EditorFactory.getInstance.getAllEditors.toSeq
      .filter(_.getProject == project)
      .flatMap(editor => Option(ScalaConsoleInfo.getConsole(editor)))
      .collect { case repl: Repl => repl }
