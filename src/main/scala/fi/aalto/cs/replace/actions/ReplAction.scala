package fi.aalto.cs.replace.actions

import com.intellij.execution.configurations.*
import com.intellij.execution.filters.TextConsoleBuilderImpl
import com.intellij.execution.runners.ExecutionEnvironment
import com.intellij.execution.ui.ConsoleView
import com.intellij.execution.{Executor, RunManagerEx}
import com.intellij.notification.{Notification, NotificationType, Notifications}
import com.intellij.openapi.actionSystem.{AnActionEvent, CommonDataKeys, DataContext}
import com.intellij.openapi.module.Module
import com.intellij.openapi.project.Project
import com.intellij.openapi.roots.ModuleRootManager
import fi.aalto.cs.replace.Repl
import fi.aalto.cs.replace.utils.MyBundle.*
import fi.aalto.cs.replace.utils.ModuleUtils
import org.jetbrains.annotations.NotNull
import org.jetbrains.plugins.scala.console.actions.RunConsoleAction
import org.jetbrains.plugins.scala.console.configuration.ScalaConsoleRunConfiguration
import org.jetbrains.plugins.scala.project.ProjectExt

/** Custom class that adjusts Scala Plugin's own RunConsoleAction with A+ requirements.
  */
class ReplAction extends RunConsoleAction:

  override def update(e: AnActionEvent): Unit =
    if e.getProject == null then return
    e.getPresentation.setEnabledAndVisible(e.getProject.hasScala)

  override def actionPerformed(@NotNull e: AnActionEvent): Unit =
    val dataContext = e.getDataContext
    val project     = CommonDataKeys.PROJECT.getData(dataContext)
    if project == null then return
    val runManagerEx = RunManagerEx.getInstanceEx(project)

    /*
     * The "priority order" is as follows:
     *   1. If a file is open in the editor, and it belongs to a module that has Scala SDK as a
     *      library dependency, start the REPL for the module of the file.
     *   2. Otherwise, if a module (or file inside a module) is selected in the project menu on the
     *      left and the module has Scala SDK as a library dependency, start the REPL for that
     *      module.
     *
     * Checking that the module has Scala SDK as a dependency is done to avoid the "no Scala facet
     * configured for module" error.
     */
    val selectedModule = getScalaModuleOfEditorFile(project, dataContext)
      .orElse(getScalaModuleOfSelectedFile(project, dataContext))

    val setting = runManagerEx.createConfiguration(
      message("ui.repl.console.scala.repl"),
      new ReplConfigurationFactory()
    )
    val configuration = setting.getConfiguration.asInstanceOf[ScalaConsoleRunConfiguration]

    selectedModule match
      case Some(module) => setConfigurationConditionally(project, module, configuration)
      case None =>
        Notifications.Bus.notify(
          Notification(
            "REPLace",
            message("ui.repl.notification.notFound.title"),
            message("ui.repl.notification.notFound.message"),
            NotificationType.WARNING
          )
        )
        super.actionPerformed(e) // Delegate to the original Scala Plugin REPL
        return
    RunConsoleAction.runExisting(setting, runManagerEx, project)

  private class ReplConfigurationFactory extends ConfigurationFactory(getMyConfigurationType):
    override def createTemplateConfiguration(project: Project): ScalaConsoleRunConfiguration =
      new ReplConfiguration(project, this, message("ui.repl.console.scala.repl"))

    override def getId: String = message("ui.repl.console.scala.repl.extended")

    // scalastyle:off no.clone
    private class ReplConfiguration(
        project: Project,
        configurationFactory: ConfigurationFactory,
        name: String
    ) extends ScalaConsoleRunConfiguration(project, configurationFactory, name):

      private def getModule: Option[Module] = Option(getConfigurationModule.getModule)

      override def getState(executor: Executor, env: ExecutionEnvironment): RunProfileState =
        val state = super.getState(executor, env).asInstanceOf[JavaCommandLineState]

        getModule match
          case Some(module) =>
            state.setConsoleBuilder(new MyBuilder(module))
          case None =>

        state

      override def clone(): ModuleBasedConfiguration[? <: RunConfigurationModule, ?] = super.clone()

      private class MyBuilder(module: Module) extends TextConsoleBuilderImpl(module.getProject):
        override def createConsole(): ConsoleView = new Repl(module)

  private def getReplAdditionalArguments(@NotNull project: Project): String =
    Repl.additionalArguments(project)

  private def setConfigurationFields(
      @NotNull configuration: ScalaConsoleRunConfiguration,
      @NotNull workingDirectory: String,
      @NotNull module: Module
  ): Unit =
    configuration.workingDirectory = workingDirectory
    configuration.setModule(module)
    configuration.setName(message("ui.repl.console.name", module.getName))

    var args = "-usejavacp " + getReplAdditionalArguments(module.getProject)

    // Scala 3 no longer has an option for preloading REPL commands, so there's no point
    // in adding this command-line switch anymore
    // Instead, we use an alternative method of preloading commands by using ScalaExecutor
    if !ModuleUtils.isScala3Module(module) then
      ModuleUtils.createInitialReplCommandsFile(module)
      if ModuleUtils.initialReplCommandsFileExists(module) then
        args += " -i " + Repl.initialCommandsFileName

    configuration.myConsoleArgs = args

  /** Sets configuration fields for the given configuration.
    */
  private def setConfigurationConditionally(
      @NotNull project: Project,
      @NotNull module: Module,
      @NotNull configuration: ScalaConsoleRunConfiguration
  ): Unit = setConfigurationFields(configuration, ModuleUtils.getModuleDirectory(module), module)

  private def getScalaModuleOfEditorFile(
      @NotNull project: Project,
      @NotNull context: DataContext
  ): Option[Module] =
    ModuleUtils.getModuleOfEditorFile(project, context).filter(hasScalaSdkLibrary)

  private def getScalaModuleOfSelectedFile(
      @NotNull project: Project,
      @NotNull context: DataContext
  ): Option[Module] =
    ModuleUtils.getModuleOfSelectedFile(project, context).filter(hasScalaSdkLibrary)

  private def hasScalaSdkLibrary(@NotNull module: Module): Boolean = ModuleUtils.nonEmpty(
    ModuleRootManager
      .getInstance(module)
      .orderEntries()
      .librariesOnly()
      .satisfying(_.getPresentableName.contains("scala-sdk-"))
  )
