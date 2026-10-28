package org.orbeon.msv.iso_relax.verifier

import java.io.{File, IOException}
import org.w3c.dom.Node
import org.xml.sax.{EntityResolver, ErrorHandler, InputSource, SAXException, SAXNotRecognizedException, SAXNotSupportedException}

object Verifier {
  val FEATURE_HANDLER: String = "http://www.iso_relax.org/verifier/handler"
  val FEATURE_FILTER: String  = "http://www.iso_relax.org/verifier/filter"
}

trait Verifier {
  @throws(classOf[SAXNotRecognizedException])
  @throws(classOf[SAXNotSupportedException])
  def isFeature(feature: String): Boolean

  @throws(classOf[SAXNotRecognizedException])
  @throws(classOf[SAXNotSupportedException])
  def setFeature(feature: String, value: Boolean): Unit

  @throws(classOf[SAXNotRecognizedException])
  @throws(classOf[SAXNotSupportedException])
  def getProperty(property: String): AnyRef

  @throws(classOf[SAXNotRecognizedException])
  @throws(classOf[SAXNotSupportedException])
  def setProperty(property: String, value: AnyRef): Unit

  def setErrorHandler(handler: ErrorHandler): Unit
  def setEntityResolver(resolver: EntityResolver): Unit

  @throws(classOf[SAXException])
  @throws(classOf[IOException])
  def verify(uri: String): Boolean

  @throws(classOf[SAXException])
  @throws(classOf[IOException])
  def verify(source: InputSource): Boolean

  @throws(classOf[SAXException])
  @throws(classOf[IOException])
  def verify(file: File): Boolean

  @throws(classOf[SAXException])
  def verify(node: Node): Boolean

  @throws(classOf[SAXException])
  def getVerifierHandler: VerifierHandler

  @throws(classOf[SAXException])
  def getVerifierFilter: VerifierFilter
}
