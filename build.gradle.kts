plugins {
    id("org.jetbrains.intellij.platform") version "2.17.0"
    id("scala")
}

group = "fi.aalto.cs.replace"
version = "1.2.2"

repositories {
    mavenCentral()

    intellijPlatform {
        defaultRepositories()
    }
}

dependencies {
    intellijPlatform {
        intellijIdeaUltimate("2026.1.3")
        bundledPlugin("com.intellij.java")
        bundledPlugin("com.intellij.properties")
        compatiblePlugin("org.intellij.scala")
    }

    compileOnly("org.scala-lang:scala3-library_3:3.7.1")
}
intellijPlatform {
    pluginConfiguration {
        ideaVersion {
            sinceBuild = "261"
        }
    }
}

tasks.withType<Jar> {
    duplicatesStrategy = DuplicatesStrategy.WARN
}
