package fi.aalto.cs.replace.intellij.actions

import com.intellij.execution.configurations.*
import com.intellij.execution.filters.TextConsoleBuilderImpl
import com.intellij.execution.runners.ExecutionEnvironment
import com.intellij.execution.ui.ConsoleView
import com.intellij.execution.{Executor, RunManagerEx}
import com.intellij.openapi.actionSystem.{AnActionEvent, CommonDataKeys, DataContext}
import com.intellij.openapi.module.{Module, ModuleManager}
import com.intellij.openapi.project.Project
import com.intellij.openapi.roots.ModuleRootManager
import com.intellij.openapi.util.io.FileUtilRt.toSystemIndependentName
import fi.aalto.cs.replace.intellij.Repl
import fi.aalto.cs.replace.intellij.services.PluginSettings
import fi.aalto.cs.replace.intellij.services.PluginSettings.MODULE_REPL_INITIAL_COMMANDS_FILE_NAME
import fi.aalto.cs.replace.intellij.utils.ModuleUtils
import fi.aalto.cs.replace.presentation.ReplConfigurationFormModel
import fi.aalto.cs.replace.ui.repl.{ReplConfigurationDialog, ReplConfigurationForm}
import fi.aalto.cs.replace.utils.PluginResourceBundle.{getAndReplaceText, getText}
import org.jetbrains.annotations.NotNull
import org.jetbrains.plugins.scala.actions.ScalaActionUtil
import org.jetbrains.plugins.scala.console.actions.RunConsoleAction
import org.jetbrains.plugins.scala.console.configuration.ScalaConsoleRunConfiguration
import org.jetbrains.plugins.scala.project.ProjectExt

import scala.jdk.CollectionConverters.ListHasAsScala

/**
 * Custom class that adjusts Scala Plugin's own RunConsoleAction with A+ requirements.
 */
class ReplAction extends RunConsoleAction:

  override def update(e: AnActionEvent): Unit =
    if e.getProject == null || e.getProject.isDisposed then return

    if e.getProject.hasScala then ScalaActionUtil.enablePresentation(e) else ScalaActionUtil.disablePresentation(e)
  end update


  override def actionPerformed(@NotNull e: AnActionEvent): Unit =
    val dataContext = e.getDataContext
    val project = CommonDataKeys.PROJECT.getData(dataContext)

    if project == null then return // scalastyle:ignore

    val runManagerEx = RunManagerEx.getInstanceEx(project)

    /*
     * The "priority order" is as follows:
     *   1. If a file is open in the editor and it belongs to a module that has Scala SDK as a
     *      library dependency, start the REPL for the module of the file.
     *   2. Otherwise if a module (or file inside a module) is selected in the project menu on the
     *      left and the module has Scala SDK as a library dependency, start the REPL for that
     *      module.
     *   3. Otherwise start a project level REPL
     *
     * Checking that the module has Scala SDK as a dependency is done to avoid the "no Scala facet
     * configured for module" error.
     */
    val selectedModule = getScalaModuleOfEditorFile(project, dataContext)
      .orElse(getScalaModuleOfSelectedFile(project, dataContext)).orElse(getDefaultModule(project))

    val setting = runManagerEx.createConfiguration(
      getText("ui.repl.console.scala.repl"), new ReplConfigurationFactory())
    val configuration = setting.getConfiguration.asInstanceOf[ScalaConsoleRunConfiguration]

    selectedModule match
      case Some(module) =>
        if setConfigurationConditionally(project, module, configuration) then
          RunConsoleAction.runExisting(setting, runManagerEx, project)
      case None =>
        super.actionPerformed(e) // Delegate to the original Scala Plugin REPL
  end actionPerformed

  private class ReplConfigurationFactory() extends ConfigurationFactory(getMyConfigurationType):
    override def createTemplateConfiguration(project: Project): ScalaConsoleRunConfiguration =
      new ReplConfiguration(project, this,
        getText("ui.repl.console.scala.repl"))

    override def getId: String = getText("ui.repl.console.scala.repl.extended")

    // scalastyle:off no.clone
    private class ReplConfiguration(project: Project,
                                    configurationFactory: ConfigurationFactory,
                                    name: String)
      extends ScalaConsoleRunConfiguration(project, configurationFactory, name):

      private def getModule: Option[Module] = Option(getConfigurationModule.getModule)

      override def getState(executor: Executor, env: ExecutionEnvironment): RunProfileState =
        val state = super.getState(executor, env).asInstanceOf[JavaCommandLineState]

        getModule match
          case Some(module) =>
            state.setConsoleBuilder(new MyBuilder(module))
          case None =>

        state

      override def clone(): ModuleBasedConfiguration[_ <: RunConfigurationModule, _] = super.clone()

      private class MyBuilder(module: Module) extends TextConsoleBuilderImpl(module.getProject):
        override def createConsole(): ConsoleView = new Repl(module)

    // scalastyle:on no.clone

  def getDefaultModule(@NotNull project: Project): Option[Module] =
    PluginSettings.getMainViewModel(project).courseViewModel match
      case Some(courseViewModel) =>
        Option(ModuleManager.getInstance(project).findModuleByName(
          courseViewModel
            .model
            .autoInstallComponentNames
            //  we, hereby, commonly agree, that the first in the list auto install component (module)
            //  is ultimately REPL's default module (as it's most likely to exist). sorry :pensive:
            .head))
      case None => None

  def getReplAdditionalArguments(@NotNull project: Project): String =
    PluginSettings.getMainViewModel(project).courseViewModel match
      case Some(courseViewModel) => courseViewModel.model.replAdditionalArguments
      case None => ""

  def setConfigurationFields(@NotNull configuration: ScalaConsoleRunConfiguration,
                             @NotNull workingDirectory: String,
                             @NotNull module: Module): Unit =
    configuration.setWorkingDirectory(workingDirectory)
    configuration.setModule(module)
    configuration.setName(getAndReplaceText("ui.repl.console.name", module.getName))

    var args = "-usejavacp " + getReplAdditionalArguments(module.getProject)

    // Scala 3 no longer has an option for preloading REPL commands, so there's no point
    // in adding this command-line switch anymore
    // Instead, we use an alternative method of preloading commands by using ScalaExecutor
    if !ModuleUtils.isScala3Module(module) then
      ModuleUtils.createInitialReplCommandsFile(module)
      if ModuleUtils.initialReplCommandsFileExists(module) then
        args += " -i " + MODULE_REPL_INITIAL_COMMANDS_FILE_NAME

    configuration.setMyConsoleArgs(args)

  /**
   * Sets configuration fields for the given configuration and returns true. Returns false if
   * the REPL start is cancelled (i.e. user selects "Cancel" in the REPL configuration dialog).
   */
  def setConfigurationConditionally(@NotNull project: Project,
                                    @NotNull module: Module,
                                    @NotNull configuration: ScalaConsoleRunConfiguration): Boolean =


    if PluginSettings.shouldShowReplConfigurationDialog then
      setConfigurationFieldsFromDialog(configuration, project, module)
    else
      setConfigurationFields(configuration, ModuleUtils.getModuleDirectory(module), module)
      true

  /**
   * Sets the configuration fields from the REPL dialog. Returns true if it is done successfully,
   * and false if the user cancels the REPL dialog.
   */
  private def setConfigurationFieldsFromDialog(@NotNull configuration: ScalaConsoleRunConfiguration,
                                               @NotNull project: Project,
                                               @NotNull module: Module): Boolean =
    val configModel = showReplDialog(project, module)
    if !configModel.isStartRepl then
      false
    else
      val changedModuleName = configModel.getTargetModuleName
      val changedModule = ModuleManager.getInstance(project).findModuleByName(changedModuleName)
      val changedWorkDir = toSystemIndependentName(configModel.getModuleWorkingDirectory)
      setConfigurationFields(configuration, changedWorkDir, changedModule)
      true

  private def showReplDialog(@NotNull project: Project,
                             @NotNull module: Module): ReplConfigurationFormModel =
    val configModel = new ReplConfigurationFormModel(project, ModuleUtils.getModuleDirectory(module), module.getName)
    val configForm = new ReplConfigurationForm(configModel, project)
    val configDialog = new ReplConfigurationDialog
    configDialog.setReplConfigurationForm(configForm)
    configDialog.setVisible(true)
    configModel

  private def getScalaModuleOfEditorFile(@NotNull project: Project,
                                         @NotNull context: DataContext): Option[Module] =
    ModuleUtils.getModuleOfEditorFile(project, context).filter(hasScalaSdkLibrary)

  private def getScalaModuleOfSelectedFile(@NotNull project: Project,
                                           @NotNull context: DataContext): Option[Module] =
    ModuleUtils.getModuleOfSelectedFile(project, context).filter(hasScalaSdkLibrary)

  private def hasScalaSdkLibrary(@NotNull module: Module): Boolean = ModuleUtils.nonEmpty(
    ModuleRootManager.getInstance(module)
      .orderEntries()
      .librariesOnly()
      .satisfying(_.getPresentableName.startsWith("scala-sdk-")))


