package fi.aalto.cs.replace

import com.intellij.testFramework.fixtures.BasePlatformTestCase
import fi.aalto.cs.replace.services.ReplChangesService
import org.junit.Assert.{assertFalse, assertTrue}

class ReplChangesServiceTest extends BasePlatformTestCase:

  private def service = ReplChangesService(getProject)

  def testModuleIsUnchangedWhenAReplStarts(): Unit =
    service.onReplStarted(getModule)
    try assertFalse(service.hasModuleChanged(getModule))
    finally service.onReplClosed(getModule)

  def testDocumentChangeMarksTheModuleOfTheEditedFile(): Unit =
    service.onReplStarted(getModule)
    try
      myFixture.configureByText("Example.scala", "object Example")
      myFixture.`type`("x")
      assertTrue(service.hasModuleChanged(getModule))
    finally service.onReplClosed(getModule)

  def testStartingANewReplClearsThePendingChange(): Unit =
    service.onReplStarted(getModule)
    try
      myFixture.configureByText("Example.scala", "object Example")
      myFixture.`type`("x")
      assertTrue(service.hasModuleChanged(getModule))

      service.onReplStarted(getModule)
      assertFalse(service.hasModuleChanged(getModule))
      service.onReplClosed(getModule)
    finally service.onReplClosed(getModule)

  def testChangesAreIgnoredWhileNoReplIsOpen(): Unit =
    service.onReplStarted(getModule)
    service.onReplClosed(getModule)

    myFixture.configureByText("Example.scala", "object Example")
    myFixture.`type`("x")
    assertFalse(service.hasModuleChanged(getModule))
