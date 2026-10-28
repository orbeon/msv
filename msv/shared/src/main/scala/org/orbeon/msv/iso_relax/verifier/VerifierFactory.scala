package org.orbeon.msv.iso_relax.verifier

import java.io.{File, IOException, InputStream}
import java.util.concurrent.CopyOnWriteArrayList
import org.xml.sax.{EntityResolver, InputSource, SAXException, SAXNotRecognizedException}
import scala.compiletime.uninitialized

abstract class VerifierFactory {
  private var resolver: EntityResolver = uninitialized

  @throws(classOf[VerifierConfigurationException])
  @throws(classOf[SAXException])
  @throws(classOf[IOException])
  def newVerifier(uri: String): Verifier = compileSchema(uri).newVerifier()

  @throws(classOf[VerifierConfigurationException])
  @throws(classOf[SAXException])
  @throws(classOf[IOException])
  def newVerifier(file: File): Verifier = compileSchema(file).newVerifier()

  @throws(classOf[VerifierConfigurationException])
  @throws(classOf[SAXException])
  @throws(classOf[IOException])
  def newVerifier(stream: InputStream): Verifier = compileSchema(stream, null).newVerifier()

  @throws(classOf[VerifierConfigurationException])
  @throws(classOf[SAXException])
  @throws(classOf[IOException])
  def newVerifier(stream: InputStream, systemId: String): Verifier = compileSchema(stream, systemId).newVerifier()

  @throws(classOf[VerifierConfigurationException])
  @throws(classOf[SAXException])
  @throws(classOf[IOException])
  def newVerifier(source: InputSource): Verifier = compileSchema(source).newVerifier()

  @throws(classOf[VerifierConfigurationException])
  @throws(classOf[SAXException])
  @throws(classOf[IOException])
  def compileSchema(is: InputSource): Schema

  @throws(classOf[VerifierConfigurationException])
  @throws(classOf[SAXException])
  @throws(classOf[IOException])
  def compileSchema(url: String): Schema = compileSchema(new InputSource(url))

  @throws(classOf[VerifierConfigurationException])
  @throws(classOf[SAXException])
  @throws(classOf[IOException])
  def compileSchema(stream: InputStream): Schema = compileSchema(stream, null)

  @throws(classOf[VerifierConfigurationException])
  @throws(classOf[SAXException])
  @throws(classOf[IOException])
  def compileSchema(stream: InputStream, systemId: String): Schema = {
    val is = new InputSource(stream)
    if (systemId != null) is.setSystemId(systemId)
    compileSchema(is)
  }

  @throws(classOf[VerifierConfigurationException])
  @throws(classOf[SAXException])
  @throws(classOf[IOException])
  def compileSchema(file: File): Schema = {
    var uri = "file:" + file.getAbsolutePath
    if (File.separatorChar == '\\')
      uri = uri.replace('\\', '/')
    compileSchema(new InputSource(uri))
  }

  @throws(classOf[SAXNotRecognizedException])
  def isFeature(feature: String): Boolean = {
    if (Verifier.FEATURE_HANDLER == feature || Verifier.FEATURE_FILTER == feature)
      true
    else
      throw new SAXNotRecognizedException(feature)
  }

  @throws(classOf[SAXNotRecognizedException])
  def setFeature(feature: String, value: Boolean): Unit =
    throw new SAXNotRecognizedException(feature)

  @throws(classOf[SAXNotRecognizedException])
  def getProperty(property: String): AnyRef =
    throw new SAXNotRecognizedException(property)

  @throws(classOf[SAXNotRecognizedException])
  def setProperty(property: String, value: AnyRef): Unit =
    throw new SAXNotRecognizedException(property)

  def setEntityResolver(resolver: EntityResolver): Unit =
    this.resolver = resolver

  def getEntityResolver: EntityResolver = resolver
}

object VerifierFactory {
  private val registeredLoaders = new CopyOnWriteArrayList[VerifierFactoryLoader]()

  def registerLoader(loader: VerifierFactoryLoader): Unit =
    registeredLoaders.add(loader)

  @throws(classOf[VerifierConfigurationException])
  def newInstance(): VerifierFactory =
    newInstance("http://www.xml.gr.jp/xmlns/relaxNamespace")

  @throws(classOf[VerifierConfigurationException])
  def newInstance(language: String): VerifierFactory =
    findFactory(language) match {
      case Some(f) => f
      case None    => throw new VerifierConfigurationException("no validation engine available for: " + language)
    }

  private def findFactory(language: String): Option[VerifierFactory] = {
    val it = registeredLoaders.iterator()
    while (it.hasNext) {
      try {
        val factory = it.next().createFactory(language)
        if (factory != null) return Some(factory)
      } catch {
        case _: Throwable =>
      }
    }
    VerifierFactoryPlatform.findFactory(language)
  }
}
