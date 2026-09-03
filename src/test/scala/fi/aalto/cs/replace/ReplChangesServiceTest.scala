package fi.aalto.cs.replace

import com.intellij.execution.ui.ConsoleViewContentType
import com.intellij.openapi.actionSystem.CommonDataKeys
import com.intellij.openapi.actionSystem.impl.SimpleDataContext
import com.intellij.openapi.application.WriteAction
import com.intellij.openapi.command.WriteCommandAction
import com.intellij.openapi.fileEditor.FileDocumentManager
import com.intellij.openapi.roots.ModuleRootManager
import com.intellij.openapi.vfs.VirtualFile
import com.intellij.testFramework.TestActionEvent
import fi.aalto.cs.replace.services.ReplChangesService
import org.jetbrains.plugins.scala.console.replace.ConsoleExecuteAction
import org.jetbrains.plugins.scala.extensions.inWriteAction
import org.junit.Assert.{assertFalse, assertTrue}
import org.junit.jupiter.api.Test

import java.io.IOException
import java.nio.file.{Files, Path}

class ReplChangesServiceTest extends ReplPlatformTestBase:

  private def service = ReplChangesService(getProject)

  /** Types into a file that belongs to the module, firing the document-change events the service
    * listens for.
    */
  private def typeIntoAFileOfTheModule(name: String = "Example.scala"): Unit =
    val root = ModuleRootManager.getInstance(getModule).getContentRoots.head
    val file = WriteAction.compute[VirtualFile, IOException](() => root.createChildData(this, name))
    val document = FileDocumentManager.getInstance.getDocument(file)
    WriteCommandAction.runWriteCommandAction(
      getProject,
      (() => document.insertString(0, "object Example")): Runnable
    )

  @Test
  def testDocumentChangeMarksTheModuleOfTheEditedFile(): Unit =
    service.onReplStarted(getModule)
    try
      typeIntoAFileOfTheModule()
      assertTrue(service.hasModuleChanged(getModule))
    finally service.onReplClosed(getModule)

  @Test
  def testStartingANewReplClearsThePendingChange(): Unit =
    service.onReplStarted(getModule)
    try
      typeIntoAFileOfTheModule()
      assertTrue(service.hasModuleChanged(getModule))

      service.onReplStarted(getModule)
      assertFalse(service.hasModuleChanged(getModule))
      service.onReplClosed(getModule)
    finally service.onReplClosed(getModule)

  @Test
  def testChangesAreStillTrackedWhileASecondReplRemainsOpen(): Unit =
    // Two REPLs on one module: closing one must not stop tracking for the other.
    service.onReplStarted(getModule)
    service.onReplStarted(getModule)
    service.onReplClosed(getModule)
    try
      typeIntoAFileOfTheModule()
      assertTrue(service.hasModuleChanged(getModule))
    finally service.onReplClosed(getModule)

  @Test
  def testChangesAreIgnoredWhileNoReplIsOpen(): Unit =
    service.onReplStarted(getModule)
    service.onReplClosed(getModule)

    typeIntoAFileOfTheModule()
    assertFalse(service.hasModuleChanged(getModule))

  @Test
  def testAConsoleOfACoursesProjectBracketsTheTracking(): Unit =
    // The console itself opens and closes the tracking, so that the banner it carries can tell the
    // user their code has moved on since this REPL was started.
    markProjectAsACoursesProject()
    withRepl(newConsole) { (_, _) =>
      typeIntoAFileOfTheModule("First.scala")
      assertTrue("an open console must track its module", service.hasModuleChanged(getModule))
    }

    withRepl(newConsole) { (_, _) =>
      assertFalse(
        "a new console starts from code it has loaded",
        service.hasModuleChanged(getModule)
      )
    }
    // Both consoles are disposed by now, so nothing is left to warn about anything.
    typeIntoAFileOfTheModule("Second.scala")
    assertFalse("a closed console must stop tracking", service.hasModuleChanged(getModule))

  @Test
  def testAConsoleOutsideACoursesProjectTracksNothing(): Unit =
    withRepl(newConsole) { (_, _) =>
      typeIntoAFileOfTheModule()
      assertFalse(
        "change tracking exists for the course banner alone",
        service.hasModuleChanged(getModule)
      )
    }

  @Test
  def testRunningInputRefreshesTheChangeBannerOfAConsoleThatNeverHadFocus(): Unit =
    // The banner is otherwise refreshed only when the console gains focus, and a student who runs
    // a selection or presses Enter from the editor never moves focus into the console.
    markProjectAsACoursesProject()
    withRepl(newConsole) { (repl, process) =>
      process.startNotify()
      repl.print(Repl.welcomeLine, ConsoleViewContentType.NORMAL_OUTPUT)
      repl.print("scala> ", ConsoleViewContentType.NORMAL_OUTPUT)
      typeIntoAFileOfTheModule("Edited.scala")
      assertFalse("nothing has refreshed the banner yet", repl.isChangeBannerVisible)

      inWriteAction(repl.getEditorDocument.setText("1 + 1"))
      val context = SimpleDataContext.getSimpleContext(CommonDataKeys.EDITOR, repl.getConsoleEditor)
      new ConsoleExecuteAction().actionPerformed(TestActionEvent.createTestEvent(context))

      assertTrue("running input must refresh the banner", repl.isChangeBannerVisible)
    }

  /** A console of the kind the run configuration builds, minus the module's Scala SDK. */
  private def newConsole: Repl =
    new Repl(getModule):
      override private[replace] def initialCommandsFile: Option[Path] = None
      override private[replace] def scala3Module: Boolean             = true

  /** Writes the marker file that makes [[Repl]] treat this project as an A+ course project. */
  private def markProjectAsACoursesProject(): Unit =
    val ideaDirectory = Path.of(getProject.getBasePath, ".idea")
    Files.createDirectories(ideaDirectory)
    Files.writeString(ideaDirectory.resolve("aplus_project.xml"), "<project/>")
