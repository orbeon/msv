package org.orbeon.msv.reader

import org.xml.sax.Attributes

/**
 * state that ignores the entire subtree.
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class IgnoreState extends State:
  private var depth: Int = 1

  final def startElement(namespaceURI: String, localName: String, qName: String, atts: Attributes): Unit =
    depth += 1

  final def endElement(namespaceURI: String, localName: String, qName: String): Unit =
    depth -= 1
    if depth == 0 then reader.popState()

  override def endDocument(): Unit =
    reader.popState()

  override def characters(buffer: Array[Char], from: Int, len: Int): Unit = ()
