package fi.aalto.cs.replace.utils

import com.intellij.DynamicBundle
import kotlin.jvm.JvmStatic
import org.jetbrains.annotations.{Nls, PropertyKey}

object MyBundle extends DynamicBundle("messages.resources"):
  @JvmStatic
  @Nls
  def message(@PropertyKey(resourceBundle = "messages.resources") key: String, params: Any*): String =
    getMessage(key, params*)
