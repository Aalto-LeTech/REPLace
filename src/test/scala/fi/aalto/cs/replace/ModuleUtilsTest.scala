package fi.aalto.cs.replace

import com.intellij.openapi.actionSystem.impl.SimpleDataContext
import com.intellij.openapi.actionSystem.{DataContext, PlatformCoreDataKeys}
import com.intellij.testFramework.fixtures.BasePlatformTestCase
import fi.aalto.cs.replace.utils.ModuleUtils
import org.junit.Assert.assertEquals

class ModuleUtilsTest extends BasePlatformTestCase:

  def testModuleSelectedInProjectTreeIsUsed(): Unit =
    // A module node in the project tree provides the module directly (issue #10).
    val context = SimpleDataContext.getSimpleContext(PlatformCoreDataKeys.MODULE, getModule)
    assertEquals(
      Some(getModule),
      ModuleUtils.getScalaReplModule(getProject, context, isEligible = _ => true)
    )

  def testNoModuleIsFoundInAnEmptyContext(): Unit =
    assertEquals(
      None,
      ModuleUtils.getScalaReplModule(getProject, DataContext.EMPTY_CONTEXT, isEligible = _ => true)
    )

  def testIneligibleModulesAreSkipped(): Unit =
    val context = SimpleDataContext.getSimpleContext(PlatformCoreDataKeys.MODULE, getModule)
    assertEquals(
      None,
      ModuleUtils.getScalaReplModule(getProject, context, isEligible = _ => false)
    )
