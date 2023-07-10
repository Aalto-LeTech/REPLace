package fi.aalto.cs.replace.utils

import com.intellij.openapi.project.Project
import java.text.MessageFormat
import java.util.ResourceBundle

object PluginResourceBundle:
  private val bundle = ResourceBundle.getBundle("resources")

  def getText(key: String): String = bundle.getString(key)
  
  def getAndReplaceText(key: String, arguments: String*): String =
    MessageFormat.format(bundle.getString(key), arguments*)
