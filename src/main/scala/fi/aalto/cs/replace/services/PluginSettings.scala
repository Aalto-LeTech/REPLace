package fi.aalto.cs.replace.services

import com.intellij.openapi.components.Service.Level
import com.intellij.openapi.components.*
import com.intellij.openapi.project.Project
import com.intellij.util.xmlb.XmlSerializerUtil

import java.lang
import scala.beans.BeanProperty

@Service(value = Array(Level.PROJECT))
@State(name = "REPLace Settings", storages = Array(new Storage("replace.xml")))
final class PluginSettings extends PersistentStateComponent[PluginSettings]:
  @BeanProperty
  var neverShowBanner: Boolean = false

  override def getState: PluginSettings = this

  override def loadState(s: PluginSettings): Unit = XmlSerializerUtil.copyBean(s, this)

object PluginSettings:
  def apply(project: Project): PluginSettings = project.getService(classOf[PluginSettings])