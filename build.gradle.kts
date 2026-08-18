import org.jetbrains.intellij.platform.gradle.TestFrameworkType

val scalaCompileVersion = "3.7.1"
val scalaReplTestRuntime = configurations.create("scalaReplTestRuntime")

plugins {
    id("org.jetbrains.intellij.platform") version "2.18.1"
    id("scala")
    id("com.diffplug.spotless") version "8.9.0"
}

group = "fi.aalto.cs.replace"
version = "1.2.1"

repositories {
    mavenCentral()

    intellijPlatform {
        defaultRepositories()
    }
}

dependencies {
    intellijPlatform {
        intellijIdea("2026.2.0.1")
        bundledPlugin("com.intellij.java")
        compatiblePlugin("org.intellij.scala")
        testFramework(TestFrameworkType.Platform)

        plugin("fi.aalto.cs.intellij-plugin:4.4.2")
        plugin("fi.aalto.cs.inspections:1.1.0")
    }

    compileOnly("org.scala-lang:scala3-library_3:$scalaCompileVersion")
    testImplementation("junit:junit:4.13.2")
    testImplementation("org.scala-lang:scala3-compiler_3:$scalaCompileVersion")
    scalaReplTestRuntime("org.scala-lang:scala3-repl_3:3.8.2")
    scalaReplTestRuntime("org.scala-lang:scala-library:3.8.2")
}

java {
    toolchain {
        languageVersion = JavaLanguageVersion.of(25)
    }
}

intellijPlatform {
    pluginConfiguration {
        ideaVersion {
            sinceBuild = "262"
        }
    }
}

spotless {
    scala {
        scalafmt("3.8.0").configFile(".scalafmt.conf")
    }
}

tasks.withType<Jar> {
    duplicatesStrategy = DuplicatesStrategy.WARN
}

tasks.test {
    systemProperty("replace.scalaReplTestClasspath", scalaReplTestRuntime.asPath)
    classpath = files(sourceSets.main.get().output.resourcesDir) + classpath
}
