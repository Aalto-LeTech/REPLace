package fi.aalto.cs.replace.intellij.services

import com.intellij.openapi.project.Project

object PluginSettings:
  val MODULE_REPL_INITIAL_COMMANDS_FILE_NAME = ".repl-commands";

  private val mainViewModel = MainViewModel()

  var shouldShowReplConfigurationDialog = true

  def getMainViewModel(project: Project) = mainViewModel


  class MainViewModel:
    val name = "main"
    val courseViewModel: Option[CourseViewModel] = None

  class CourseViewModel:
    val name = "course"
    val model = Course()

  class Course:
    val autoInstallComponentNames: List[String] = List()
    val replAdditionalArguments: String = ""
    val replInitialCommands: Map[String, List[String]] = Map()