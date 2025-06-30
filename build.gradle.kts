plugins {
    id("org.jetbrains.intellij.platform") version "2.6.0"
    id("scala")
}

group = "fi.aalto.cs.replace"
version = "1.0.0"

scala {
    scalaVersion = "3.7.1"
}

repositories {
    mavenCentral()

    intellijPlatform {
        defaultRepositories()
    }
}

dependencies {
    intellijPlatform {
        intellijIdeaCommunity("2025.1.3")
        bundledPlugin("com.intellij.java")
        bundledPlugin("com.intellij.properties")
        compatiblePlugin("org.intellij.scala")
    }
}
intellijPlatform {
    pluginConfiguration {
        ideaVersion {
            sinceBuild = "251"
        }
    }
}
