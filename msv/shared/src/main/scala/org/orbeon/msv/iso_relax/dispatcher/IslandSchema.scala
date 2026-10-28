package org.orbeon.msv.iso_relax.dispatcher

import java.util.Iterator
import org.xml.sax.{ErrorHandler, SAXException}

trait IslandSchema {
  def createNewVerifier(namespaceURI: String, rules: Array[ElementDecl]): IslandVerifier
  def getElementDeclByName(name: String): ElementDecl
  def iterateElementDecls(): Iterator[ElementDecl]
  def getElementDecls: Array[ElementDecl]
  def getAttributesDeclByName(name: String): AttributesDecl
  def iterateAttributesDecls(): Iterator[AttributesDecl]
  def getAttributesDecls: Array[AttributesDecl]
  def createNewAttributesVerifier(namespaceURI: String, decls: Array[AttributesDecl]): AttributesVerifier

  @throws(classOf[SAXException])
  def bind(provider: SchemaProvider, handler: ErrorHandler): Unit
}
