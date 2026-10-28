package org.orbeon.msv.util.xml

import org.w3c.dom.{Attr, Document, Element, Node}
import org.xml.sax.helpers.{AttributesImpl, LocatorImpl}
import org.xml.sax.{ContentHandler, SAXException}

/**
 * produces SAX2 event from a DOM tree.
 *
 * @author <a href="mailto:kohsuke.kawaguchi@sun.com">Kohsuke KAWAGUCHI</a>
 */
object SAXEventGenerator {
  /**
   * scans the specified DOM and sends SAX2 events to the handler.
   */
  @throws(classOf[SAXException])
  def parse(dom: Document, handler: ContentHandler): Unit = {
    val visitor = new DOMVisitor {
      override def visit(e: Element): Unit = {
        val attLen = e.getAttributes.getLength
        val atts = new AttributesImpl()
        var i = 0
        while (i < attLen) {
          val a = e.getAttributes.item(i).asInstanceOf[Attr]
          var uri = a.getNamespaceURI
          var local = a.getLocalName
          if (uri == null) uri = ""
          if (local == null) local = a.getName
          atts.addAttribute(uri, local, a.getName, "CDATA", a.getValue)
          i += 1
        }

        try {
          var uri = e.getNamespaceURI
          var local = e.getLocalName
          if (uri == null) uri = ""
          if (local == null) local = e.getNodeName

          handler.startElement(uri, local, e.getNodeName, atts)
          super.visit(e)
          handler.endElement(uri, local, e.getNodeName)
        } catch {
          case x: SAXException =>
            throw new SAXWrapper(x)
        }
      }

      override def visitNode(n: Node): Unit = {
        if (n.getNodeType == Node.TEXT_NODE || n.getNodeType == Node.CDATA_SECTION_NODE) {
          val text = n.getNodeValue
          try {
            handler.characters(text.toCharArray, 0, text.length)
          } catch {
            case x: SAXException =>
              throw new SAXWrapper(x)
          }
        }
        super.visitNode(n)
      }
    }

    handler.setDocumentLocator(new LocatorImpl())
    handler.startDocument()
    try {
      visitor.visit(dom)
    } catch {
      case w: SAXWrapper =>
        throw w.e
    }
    handler.endDocument()
  }

  private class SAXWrapper(val e: SAXException) extends RuntimeException
}
