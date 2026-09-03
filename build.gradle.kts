import org.jetbrains.intellij.platform.gradle.TestFrameworkType

val scalaCompileVersion = "3.7.1"
val scalaReplTestRuntime = configurations.create("scalaReplTestRuntime")

plugins {
    id("org.jetbrains.intellij.platform") version "2.18.1"
    id("scala")
    id("com.diffplug.spotless") version "8.9.0"
}

group = "fi.aalto.cs.replace"
version = "1.4.0"

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
        testFramework(TestFrameworkType.JUnit5)

        plugin("fi.aalto.cs.intellij-plugin:4.5.0")
        plugin("fi.aalto.cs.inspections:1.1.0")
    }

    compileOnly("org.scala-lang:scala3-library_3:$scalaCompileVersion")
    testImplementation(platform("org.junit:junit-bom:6.1.3"))
    testImplementation("org.junit.jupiter:junit-jupiter")
    testRuntimeOnly("org.junit.platform:junit-platform-launcher")

    // https://plugins.jetbrains.com/docs/intellij/tools-intellij-platform-gradle-plugin-faq.html#junit5-test-framework-refers-to-junit4
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
    // plugin.xml registers no Configurable, so this only boots a headless IDE on every
    // buildPlugin to index nothing.
    buildSearchableOptions = false
    // Only rewrites JetBrains @NotNull and compiles GUI-Designer forms; a no-op for Scala-only
    // sources.
    instrumentCode = false

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
    useJUnitPlatform()
    systemProperty("replace.scalaReplTestClasspath", scalaReplTestRuntime.asPath)
    // The code-provenance plugin's background flows race project disposal and get reported as
    // uncaught errors in whatever test happens to be running; irrelevant to this plugin.
    systemProperty("intellij.code.provenance.enabled", "false")
    classpath = files(sourceSets.main.get().output.resourcesDir) + classpath
}

tasks.check {
    dependsOn(tasks.verifyPluginProjectConfiguration)
}
