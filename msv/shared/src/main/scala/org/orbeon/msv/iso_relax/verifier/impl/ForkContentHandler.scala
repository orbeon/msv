package org.orbeon.msv.iso_relax.verifier.impl

import org.xml.sax.{Attributes, ContentHandler, Locator, SAXException}

class ForkContentHandler(first: ContentHandler, second: ContentHandler) extends ContentHandler {
  private val lhs: ContentHandler = first
  private val rhs: ContentHandler = second

  def setDocumentLocator(locator: Locator): Unit = {
    lhs.setDocumentLocator(locator)
    rhs.setDocumentLocator(locator)
  }

  @throws(classOf[SAXException])
  def startDocument(): Unit = {
    lhs.startDocument()
    rhs.startDocument()
  }

  @throws(classOf[SAXException])
  def endDocument(): Unit = {
    lhs.endDocument()
    rhs.endDocument()
  }

  @throws(classOf[SAXException])
  def startPrefixMapping(prefix: String, uri: String): Unit = {
    lhs.startPrefixMapping(prefix, uri)
    rhs.startPrefixMapping(prefix, uri)
  }

  @throws(classOf[SAXException])
  def endPrefixMapping(prefix: String): Unit = {
    lhs.endPrefixMapping(prefix)
    rhs.endPrefixMapping(prefix)
  }

  @throws(classOf[SAXException])
  def startElement(uri: String, localName: String, qName: String, attributes: Attributes): Unit = {
    lhs.startElement(uri, localName, qName, attributes)
    rhs.startElement(uri, localName, qName, attributes)
  }

  @throws(classOf[SAXException])
  def endElement(uri: String, localName: String, qName: String): Unit = {
    lhs.endElement(uri, localName, qName)
    rhs.endElement(uri, localName, qName)
  }

  @throws(classOf[SAXException])
  def characters(ch: Array[Char], start: Int, length: Int): Unit = {
    lhs.characters(ch, start, length)
    rhs.characters(ch, start, length)
  }

  @throws(classOf[SAXException])
  def ignorableWhitespace(ch: Array[Char], start: Int, length: Int): Unit = {
    lhs.ignorableWhitespace(ch, start, length)
    rhs.ignorableWhitespace(ch, start, length)
  }

  @throws(classOf[SAXException])
  def processingInstruction(target: String, data: String): Unit = {
    lhs.processingInstruction(target, data)
    rhs.processingInstruction(target, data)
  }

  @throws(classOf[SAXException])
  def skippedEntity(name: String): Unit = {
    lhs.skippedEntity(name)
    rhs.skippedEntity(name)
  }
}

object ForkContentHandler {
  def create(handlers: Array[ContentHandler]): ContentHandler = {
    if (handlers.length == 0)
      throw new IllegalArgumentException()
    if (handlers.length == 1)
      handlers(0)
    else {
      var cur = handlers(0)
      var i = 1
      while (i < handlers.length) {
        cur = new ForkContentHandler(cur, handlers(i))
        i += 1
      }
      cur
    }
  }
}
