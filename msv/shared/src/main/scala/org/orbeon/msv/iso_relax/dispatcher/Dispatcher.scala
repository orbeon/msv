package org.orbeon.msv.iso_relax.dispatcher

import org.xml.sax.{ErrorHandler, SAXException, XMLReader}

object Dispatcher {
  class NotationDecl(val name: String, val publicId: String, val systemId: String)
  class UnparsedEntityDecl(val name: String, val publicId: String, val systemId: String, val notation: String)
}

trait Dispatcher {
  def attachXMLReader(reader: XMLReader): Unit

  @throws(classOf[SAXException])
  def switchVerifier(newVerifier: IslandVerifier): Unit

  def setErrorHandler(handler: ErrorHandler): Unit
  def getErrorHandler: ErrorHandler
  def getSchemaProvider: SchemaProvider
  def countNotationDecls(): Int
  def getNotationDecl(index: Int): Dispatcher.NotationDecl
  def countUnparsedEntityDecls(): Int
  def getUnparsedEntityDecl(index: Int): Dispatcher.UnparsedEntityDecl
}
