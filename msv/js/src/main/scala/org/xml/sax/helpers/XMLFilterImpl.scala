package org.xml.sax.helpers

import org.xml.sax.*

class XMLFilterImpl(private var parent: XMLReader)
  extends XMLFilter with ContentHandler with DTDHandler with EntityResolver with ErrorHandler {

  def this() = this(null)

  private var locator: Locator = null
  private var entityResolver: EntityResolver = null
  private var dtdHandler: DTDHandler = null
  private var contentHandler: ContentHandler = null
  private var errorHandler: ErrorHandler = null

  def setParent(parent: XMLReader): Unit = this.parent = parent
  def getParent: XMLReader = parent

  def setFeature(name: String, value: Boolean): Unit =
    if (parent != null) parent.setFeature(name, value)
    else throw new SAXNotRecognizedException("Feature: " + name)

  def getFeature(name: String): Boolean =
    if (parent != null) parent.getFeature(name)
    else throw new SAXNotRecognizedException("Feature: " + name)

  def setProperty(name: String, value: AnyRef): Unit =
    if (parent != null) parent.setProperty(name, value)
    else throw new SAXNotRecognizedException("Property: " + name)

  def getProperty(name: String): AnyRef =
    if (parent != null) parent.getProperty(name)
    else throw new SAXNotRecognizedException("Property: " + name)

  def setEntityResolver(resolver: EntityResolver): Unit = this.entityResolver = resolver
  def getEntityResolver: EntityResolver = entityResolver

  def setDTDHandler(handler: DTDHandler): Unit = this.dtdHandler = handler
  def getDTDHandler: DTDHandler = dtdHandler

  def setContentHandler(handler: ContentHandler): Unit = this.contentHandler = handler
  def getContentHandler: ContentHandler = contentHandler

  def setErrorHandler(handler: ErrorHandler): Unit = this.errorHandler = handler
  def getErrorHandler: ErrorHandler = errorHandler

  def parse(input: InputSource): Unit = {
    setupParse()
    parent.parse(input)
  }

  def parse(systemId: String): Unit = {
    setupParse()
    parent.parse(systemId)
  }

  private def setupParse(): Unit = {
    if (parent == null) throw new NullPointerException("No parent for filter")
    parent.setEntityResolver(this)
    parent.setDTDHandler(this)
    parent.setContentHandler(this)
    parent.setErrorHandler(this)
  }

  def resolveEntity(publicId: String, systemId: String): InputSource =
    if (entityResolver != null) entityResolver.resolveEntity(publicId, systemId) else null

  def notationDecl(name: String, publicId: String, systemId: String): Unit =
    if (dtdHandler != null) dtdHandler.notationDecl(name, publicId, systemId)

  def unparsedEntityDecl(name: String, publicId: String, systemId: String, notationName: String): Unit =
    if (dtdHandler != null) dtdHandler.unparsedEntityDecl(name, publicId, systemId, notationName)

  def setDocumentLocator(locator: Locator): Unit = {
    this.locator = locator
    if (contentHandler != null) contentHandler.setDocumentLocator(locator)
  }

  def startDocument(): Unit =
    if (contentHandler != null) contentHandler.startDocument()

  def endDocument(): Unit =
    if (contentHandler != null) contentHandler.endDocument()

  def startPrefixMapping(prefix: String, uri: String): Unit =
    if (contentHandler != null) contentHandler.startPrefixMapping(prefix, uri)

  def endPrefixMapping(prefix: String): Unit =
    if (contentHandler != null) contentHandler.endPrefixMapping(prefix)

  def startElement(uri: String, localName: String, qName: String, atts: Attributes): Unit =
    if (contentHandler != null) contentHandler.startElement(uri, localName, qName, atts)

  def endElement(uri: String, localName: String, qName: String): Unit =
    if (contentHandler != null) contentHandler.endElement(uri, localName, qName)

  def characters(ch: Array[Char], start: Int, length: Int): Unit =
    if (contentHandler != null) contentHandler.characters(ch, start, length)

  def ignorableWhitespace(ch: Array[Char], start: Int, length: Int): Unit =
    if (contentHandler != null) contentHandler.ignorableWhitespace(ch, start, length)

  def processingInstruction(target: String, data: String): Unit =
    if (contentHandler != null) contentHandler.processingInstruction(target, data)

  def skippedEntity(name: String): Unit =
    if (contentHandler != null) contentHandler.skippedEntity(name)

  def warning(e: SAXParseException): Unit =
    if (errorHandler != null) errorHandler.warning(e)

  def error(e: SAXParseException): Unit =
    if (errorHandler != null) errorHandler.error(e)

  def fatalError(e: SAXParseException): Unit =
    if (errorHandler != null) errorHandler.fatalError(e)
}
