package org.orbeon.msv.iso_relax.dispatcher

import org.xml.sax.{SAXNotRecognizedException, SAXNotSupportedException}

trait ElementDecl {
  def getName: String

  @throws(classOf[SAXNotRecognizedException])
  @throws(classOf[SAXNotSupportedException])
  def getFeature(feature: String): Boolean

  @throws(classOf[SAXNotRecognizedException])
  @throws(classOf[SAXNotSupportedException])
  def getProperty(property: String): AnyRef
}
