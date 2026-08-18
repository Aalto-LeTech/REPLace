package fi.aalto.cs.replace

import org.junit.Assert.{assertEquals, assertTrue}
import org.junit.Test

import java.util.Properties
import scala.jdk.CollectionConverters.*

/** `messages/resources.properties` appears several times on the test runtime classpath and only the
  * first copy is read, so `build.gradle.kts` pins the processed resources first. These tests fail
  * if that ordering is lost. Only tests are affected; at runtime each plugin uses its own
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
  def testBundleOnTheClasspathMatchesTheOneGradleProcessed(): Unit =
    val urls = getClass.getClassLoader.getResources(bundlePath).asScala.toList
    val processed = urls
      .find(_.toString.contains("/build/resources/"))
      .getOrElse(throw new AssertionError(s"no processResources copy of $bundlePath, found: $urls"))

    assertEquals(
      s"the bundle that will be read differs from the one Gradle processed; classpath was $urls",
      loadFrom(processed),
      loadFrom(urls.head)
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
