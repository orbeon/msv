package org.orbeon.msv.util.xml

import javax.xml.parsers.{DocumentBuilderFactory, ParserConfigurationException}
import org.w3c.dom.{Document, Element, Node}
import org.xml.sax.Attributes
import org.xml.sax.helpers.DefaultHandler

/**
 * builds DOM from SAX2 event stream.
 *
 * @author <a href="mailto:kohsuke.kawaguchi@sun.com">Kohsuke KAWAGUCHI</a>
 */
class DOMBuilder(val dom: Document) extends DefaultHandler {
  private var parent: Node = dom

  @throws(classOf[ParserConfigurationException])
  def this() = {
    this(DocumentBuilderFactory.newInstance().newDocumentBuilder().newDocument())
  }

  /**
   * returns DOM. This method should be called after the parsing was completed.
   */
  def getDocument: Document = dom

  override def startElement(ns: String, local: String, qname: String, atts: Attributes): Unit = {
    val e = dom.createElementNS(ns, qname)
    parent.appendChild(e)
    parent = e

    var i = 0
    while (i < atts.getLength) {
      e.setAttributeNS(atts.getURI(i), atts.getQName(i), atts.getValue(i))
      i += 1
    }
  }

  override def endElement(ns: String, local: String, qname: String): Unit = {
    parent = parent.getParentNode
  }

  override def characters(buf: Array[Char], start: Int, len: Int): Unit = {
    parent.appendChild(dom.createTextNode(new String(buf, start, len)))
  }

  override def ignorableWhitespace(buf: Array[Char], start: Int, len: Int): Unit = {
    parent.appendChild(dom.createTextNode(new String(buf, start, len)))
  }
}
