plugins {
    id("org.jetbrains.intellij.platform") version "2.6.0"
    id("scala")
}

group = "fi.aalto.cs.replace"
version = "1.0.0"

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

    compileOnly("org.scala-lang:scala3-library_3:3.7.1")
}
intellijPlatform {
    pluginConfiguration {
        ideaVersion {
            sinceBuild = "251"
        }
    }
}
