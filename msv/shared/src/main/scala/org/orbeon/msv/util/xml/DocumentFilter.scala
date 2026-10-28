package org.orbeon.msv.util.xml

import org.xml.sax.{AttributeList, DocumentHandler, Locator, SAXException}
import scala.annotation.nowarn

/**
 * SAX DocumentHandler event interceptor.
 *
 * This object acts as a filter to DocumentHandler events.
 * Derived class should override methods of interest and
 * perform somethings.
 *
 * @author <a href="mailto:kohsuke.kawaguchi@sun.com">Kohsuke KAWAGUCHI</a>
 */
@nowarn("cat=deprecation")
class DocumentFilter(var next: DocumentHandler) extends DocumentHandler {
  @throws(classOf[SAXException])
  def startDocument(): Unit = next.startDocument()

  @throws(classOf[SAXException])
  def endDocument(): Unit = next.endDocument()

  @throws(classOf[SAXException])
  def startElement(name: String, atts: AttributeList): Unit = next.startElement(name, atts)

  @throws(classOf[SAXException])
  def endElement(name: String): Unit = next.endElement(name)

  @throws(classOf[SAXException])
  def characters(buf: Array[Char], start: Int, len: Int): Unit = next.characters(buf, start, len)

  @throws(classOf[SAXException])
  def ignorableWhitespace(buf: Array[Char], start: Int, len: Int): Unit = next.ignorableWhitespace(buf, start, len)

  @throws(classOf[SAXException])
  def processingInstruction(target: String, data: String): Unit = next.processingInstruction(target, data)

  def setDocumentLocator(loc: Locator): Unit = next.setDocumentLocator(loc)
}
