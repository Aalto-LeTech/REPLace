package fi.aalto.cs.replace.utils

import com.intellij.DynamicBundle
import org.jetbrains.annotations.{Nls, PropertyKey}

object MyBundle:
  private val bundle = DynamicBundle(this.getClass, "messages.resources")
  @Nls
  def message(
      @PropertyKey(resourceBundle = "messages.resources") key: String,
      params: Any*
  ): String =
    bundle.getMessage(key, params*)
