package fi.aalto.cs.replace

import org.junit.Assert.assertTrue
import org.junit.jupiter.api.Test

import java.util.Properties
import scala.jdk.CollectionConverters.*

/** `messages/resources.properties` appears several times on the test runtime classpath and only the
  * first copy is read, so `build.gradle.kts` pins the processed resources first and these tests
  * fail if that ordering is lost. Only tests are affected, because each plugin has its own
  * classloader.
  */
class MessageBundleResourcesTest:

  private val bundlePath = "messages/resources.properties"

  private def loadFrom(url: java.net.URL): Properties =
    val properties = new Properties()
    val stream     = url.openStream()
    try properties.load(stream)
    finally stream.close()
    properties

  @Test
  def testBundleResolvesFromProcessedResourcesRatherThanCompilerOutput(): Unit =
    val locations = getClass.getClassLoader.getResources(bundlePath).asScala.map(_.toString).toList
    assertTrue(s"$bundlePath is not on the test runtime classpath at all", locations.nonEmpty)

    assertTrue(
      s"the winning $bundlePath must be the processResources output, classpath order was $locations",
      locations.head.contains("/build/resources/")
    )

  @Test
  def testBundleCarriesTheKeysTheUiLooksUp(): Unit =
    val properties = loadFrom(getClass.getClassLoader.getResource(bundlePath))
    List(
      "ui.repl.warning.description",
      "ui.repl.notification.notFound.title",
      "ui.repl.console.scala.repl"
    ).foreach(key =>
      assertTrue(
        s"$key missing from $bundlePath, have: ${properties.stringPropertyNames}",
        properties.containsKey(key)
      )
    )
