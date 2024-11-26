import org.jetbrains.sbtidea.Keys.*

lazy val REPLace =
  project
    .in(file("."))
    .enablePlugins(SbtIdeaPlugin)
    .settings(
      version                        := "1.1.0",
      scalaVersion                   := "3.3.3",
      ThisBuild / intellijPluginName := "REPLace",
      ThisBuild / intellijBuild      := "243.21565.193",
      ThisBuild / intellijPlatform   := IntelliJPlatform.IdeaCommunity,
      Global / intellijAttachSources := true,
      Compile / javacOptions ++= "--release" :: "21" :: Nil,
      intellijPlugins += "com.intellij.properties".toPlugin,
      intellijPlugins += "org.intellij.scala".toPlugin,
      Compile / unmanagedResourceDirectories += baseDirectory.value / "resources"
    )
