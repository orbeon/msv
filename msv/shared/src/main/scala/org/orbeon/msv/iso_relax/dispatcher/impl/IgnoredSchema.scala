package org.orbeon.msv.iso_relax.dispatcher.impl

import java.util.{ArrayList, Collections, Iterator}
import org.orbeon.msv.iso_relax.dispatcher.{AttributesDecl, AttributesVerifier, ElementDecl, IslandSchema, IslandVerifier, SchemaProvider}
import org.xml.sax.{ErrorHandler, SAXNotRecognizedException}

class IgnoredSchema extends IslandSchema {
  import IgnoredSchema._

  def getElementDeclByName(name: String): ElementDecl = elemDecl(0)

  def getElementDecls: Array[ElementDecl] = elemDecl

  def iterateElementDecls(): Iterator[ElementDecl] =
    Collections.singletonList(elemDecl(0)).iterator()

  def createNewVerifier(namespaceURI: String, rules: Array[ElementDecl]): IslandVerifier =
    new IgnoreVerifier(namespaceURI, rules)

  def getAttributesDeclByName(name: String): AttributesDecl = attrDecl(0)

  def getAttributesDecls: Array[AttributesDecl] = attrDecl

  def iterateAttributesDecls(): Iterator[AttributesDecl] =
    Collections.singletonList(attrDecl(0)).iterator()

  def createNewAttributesVerifier(namespaceURI: String, decls: Array[AttributesDecl]): AttributesVerifier =
    throw new UnsupportedOperationException("not implemented yet")

  def bind(provider: SchemaProvider, handler: ErrorHandler): Unit = ()
}

object IgnoredSchema {
  private val elemDecl: Array[ElementDecl] = Array(new ElementDecl {
    def getName: String = "$$any$$"
    def getProperty(propertyName: String): AnyRef = throw new SAXNotRecognizedException(propertyName)
    def getFeature(featureName: String): Boolean = throw new SAXNotRecognizedException(featureName)
  })

  private val attrDecl: Array[AttributesDecl] = Array(new AttributesDecl {
    def getName: String = "$$any$$"
    def getProperty(propertyName: String): AnyRef = throw new SAXNotRecognizedException(propertyName)
    def getFeature(featureName: String): Boolean = throw new SAXNotRecognizedException(featureName)
  })
}
