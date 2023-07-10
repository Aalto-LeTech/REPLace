package fi.aalto.cs.replace.presentation

import com.intellij.openapi.module.Module
import com.intellij.openapi.module.ModuleManager
import com.intellij.openapi.project.Project
import fi.aalto.cs.replace.ui.repl.ReplConfigurationForm
import java.util
import java.util.stream.Collectors
import org.jetbrains.annotations.NotNull


object ReplConfigurationFormModel:
  /**
   * Filters out the names of Scala modules for the {@link Module} array.
   *
   * @param modules {@link Module} array to process
   * @return a {@link List} of {@link String} for names of the modules
   */
  def getScalaModuleNames(modules: List[Module]): List[String] =
    modules
      .filter(module =>
        val name = module.getModuleTypeName
        //  Scala modules are of a "JAVA_MODULE" type,
        //  so it the way to distinct them from SBT-built ones.
        name != null && name == "JAVA_MODULE"
    )
      .map(_.getName)


class ReplConfigurationFormModel(private var project: Project,
                                 private var moduleWorkingDirectory: String,
                                 private var targetModuleName: String):
  private val modules = getModules(project)
  private var moduleNames: List[String] = ReplConfigurationFormModel.getScalaModuleNames(modules)
  private var startRepl = true

  /**
   * Method additionally to setting a {@link Project} updates the list of affiliated {@link Module}
   * names.
   *
   * @param project a {@link Project} to set and extract {@link Module}s to update the list of
   *                names.
   */
  def setProject(project: Project): Unit =
    this.project = project
    val modules = getModules(project)
    this.moduleNames = ReplConfigurationFormModel.getScalaModuleNames(modules)

  /**
   * Method to extract {@link Module}s from {@link Project}.
   *
   * @param project a {@link Project} to extract {@link Module}s from
   * @return an array of {@link Module}
   */
  private def getModules(project: Project) = ModuleManager.getInstance(project).getModules.toList

  def getModuleNames = moduleNames.toArray

  def getProject = project

  def isStartRepl = startRepl

  def setStartRepl(startRepl: Boolean): Unit =
    this.startRepl = startRepl

  def getModuleWorkingDirectory = moduleWorkingDirectory

  def setModuleWorkingDirectory(moduleWorkingDirectory: String): Unit =
    this.moduleWorkingDirectory = moduleWorkingDirectory

  def getTargetModuleName = targetModuleName

  def setTargetModuleName(targetModuleName: String): Unit =
    this.targetModuleName = targetModuleName
