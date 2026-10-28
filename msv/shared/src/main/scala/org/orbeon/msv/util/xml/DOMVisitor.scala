package org.orbeon.msv.util.xml

import org.w3c.dom.{Document, Element, Node, NodeList}

/**
 * visits all DOM elements in the depth-first order (in-order).
 *
 * @author <a href="mailto:kohsuke.kawaguchi@sun.com">Kohsuke KAWAGUCHI</a>
 */
abstract class DOMVisitor {
  def visit(dom: Document): Unit = {
    visit(dom.getDocumentElement)
  }

  def visit(e: Element): Unit = {
    val lst: NodeList = e.getChildNodes
    val len = lst.getLength
    var i = 0
    while (i < len) {
      val n = lst.item(i)
      if (n.getNodeType == Node.ELEMENT_NODE)
        visit(n.asInstanceOf[Element])
      else
        visitNode(n)
      i += 1
    }
  }

  /**
   * other nodes.
   */
  def visitNode(n: Node): Unit = ()
}
