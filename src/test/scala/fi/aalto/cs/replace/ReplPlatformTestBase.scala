package fi.aalto.cs.replace

import com.intellij.ide.impl.OpenProjectTask
import com.intellij.openapi.application.WriteAction
import com.intellij.openapi.module.{Module, ModuleManager}
import com.intellij.openapi.project.Project
import com.intellij.openapi.util.Disposer
import com.intellij.testFramework.junit5.fixture.{FixturesKt, TestFixture}
import com.intellij.testFramework.junit5.{RunInEdt, TestApplication}

import java.nio.file.Path

/** Base for tests that need the IntelliJ test application plus a project and module, on the JUnit 5
  * platform framework. Tests run on the EDT holding the write-intent lock through `RunInEdt`. The
  * annotation is deprecated, but its sanctioned replacement is per-method and internal API, and
  * Scala does not see Kotlin deprecations anyway.
  *
  * The fixtures are instance fields, so each test gets a fresh project and its teardown disposes
  * it. A class declaring `@TestInstance(PER_CLASS)` shares one project across its methods. The
  * module sits on a temp directory registered as a content and source root, so it looks real.
  */
@TestApplication
@RunInEdt(writeIntent = true)
abstract class ReplPlatformTestBase:
  // openAfterCreation: production code guards on `project.isOpen` (e.g. ReplChangesService).
  private val projectFixture: TestFixture[Project] =
    FixturesKt.projectFixture(FixturesKt.tempPathFixture(), OpenProjectTask.build(), true)
  private val moduleFixture: TestFixture[Module] =
    FixturesKt.moduleFixture(projectFixture, FixturesKt.tempPathFixture(), true)

  protected def getProject: Project = projectFixture.get()
  protected def getModule: Module   = moduleFixture.get()

  /** A second module, so a lookup has two answers to choose between. It needs no content of its
    * own, because only the rule that picks it is ever under test.
    */
  protected def createSecondModule(): Module =
    WriteAction.compute[Module, RuntimeException](() =>
      ModuleManager
        .getInstance(getProject)
        .newModule(Path.of(getProject.getBasePath, "other.iml"), "JAVA_MODULE")
    )

  /** Runs `test` against a live console attached to a recording process handler, disposed
    * afterwards. `getComponent` comes first, or the console has no editor to flush text into.
    */
  protected def withRepl(console: Repl)(test: (Repl, RecordingProcessHandler) => Unit): Unit =
    val process = new RecordingProcessHandler
    try
      console.getComponent()
      // attachToProcess installs and registers the console's real history controller; registering
      // a second one here would shadow it.
      console.attachToProcess(process)
      test(console, process)
    // Repl.dispose unregisters the console from the Scala plugin's registry. Repl's own flag
    // guards against a second dispose, which Disposer would rerun rather than skip.
    finally if !console.isReplDisposed then Disposer.dispose(console)
