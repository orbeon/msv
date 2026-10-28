package org.orbeon.msv.iso_relax.verifier.impl

import org.orbeon.msv.iso_relax.verifier.{Verifier, VerifierFilter, VerifierHandler}
import org.xml.sax.helpers.XMLFilterImpl
import org.xml.sax.{Attributes, EntityResolver, ErrorHandler, Locator, SAXException}

class VerifierFilterImpl(val verifier: Verifier) extends XMLFilterImpl with VerifierFilter {
  private val core: VerifierHandler = verifier.getVerifierHandler

  def isValid: Boolean = core.isValid

  override def setErrorHandler(handler: ErrorHandler): Unit = {
    super.setErrorHandler(handler)
    verifier.setErrorHandler(handler)
  }

  override def setEntityResolver(resolver: EntityResolver): Unit = {
    super.setEntityResolver(resolver)
    verifier.setEntityResolver(resolver)
  }

  override def setDocumentLocator(locator: Locator): Unit = {
    core.setDocumentLocator(locator)
    super.setDocumentLocator(locator)
  }

  @throws(classOf[SAXException])
  override def startDocument(): Unit = {
    core.startDocument()
    super.startDocument()
  }

  @throws(classOf[SAXException])
  override def endDocument(): Unit = {
    core.endDocument()
    super.endDocument()
  }

  @throws(classOf[SAXException])
  override def startPrefixMapping(prefix: String, uri: String): Unit = {
    core.startPrefixMapping(prefix, uri)
    super.startPrefixMapping(prefix, uri)
  }

  @throws(classOf[SAXException])
  override def endPrefixMapping(prefix: String): Unit = {
    core.endPrefixMapping(prefix)
    super.endPrefixMapping(prefix)
  }

  @throws(classOf[SAXException])
  override def startElement(uri: String, localName: String, qName: String, atts: Attributes): Unit = {
    core.startElement(uri, localName, qName, atts)
    super.startElement(uri, localName, qName, atts)
  }

  @throws(classOf[SAXException])
  override def endElement(uri: String, localName: String, qName: String): Unit = {
    core.endElement(uri, localName, qName)
    super.endElement(uri, localName, qName)
  }

  @throws(classOf[SAXException])
  override def characters(ch: Array[Char], start: Int, length: Int): Unit = {
    core.characters(ch, start, length)
    super.characters(ch, start, length)
  }

  @throws(classOf[SAXException])
  override def ignorableWhitespace(ch: Array[Char], start: Int, length: Int): Unit = {
    core.ignorableWhitespace(ch, start, length)
    super.ignorableWhitespace(ch, start, length)
  }

  @throws(classOf[SAXException])
  override def processingInstruction(target: String, data: String): Unit = {
    core.processingInstruction(target, data)
    super.processingInstruction(target, data)
  }

  @throws(classOf[SAXException])
  override def skippedEntity(name: String): Unit = {
    core.skippedEntity(name)
    super.skippedEntity(name)
  }
}
