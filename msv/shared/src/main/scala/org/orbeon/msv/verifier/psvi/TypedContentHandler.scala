package org.orbeon.msv.verifier.psvi

import org.orbeon.msv.grammar.{AttributeExp, ElementExp}
import org.orbeon.msv.relaxng.datatype.{Datatype, ValidationContext}
import org.xml.sax.SAXException

/**
 * Receives notification of the typed content of the document.
 *
 * <p>
 * This interface can be considered as the SAX ContentHandler plus type-information.
 * It is intended to help applications to interpret the incoming document.
 *
 * @author <a href="mailto:kohsuke.kawaguchi@sun.com">Kohsuke KAWAGUCHI</a>
 */
trait TypedContentHandler:

  /**
   * receives notification of the start of a document.
   */
  @throws(classOf[SAXException])
  def startDocument(context: ValidationContext): Unit

  /**
   * receives notification of the end of a document.
   */
  @throws(classOf[SAXException])
  def endDocument(): Unit

  /**
   * receives notification of a string.
   */
  @throws(classOf[SAXException])
  def characterChunk(literal: String, `type`: Datatype): Unit

  /**
   * receives notification of the start of an element.
   */
  @throws(classOf[SAXException])
  def startElement(namespaceURI: String, localName: String, qName: String): Unit

  /**
   * receives notification of the end of an element.
   */
  @throws(classOf[SAXException])
  def endElement(namespaceURI: String, localName: String, qName: String, `type`: ElementExp): Unit

  /**
   * receives notification of the start of an attribute.
   */
  @throws(classOf[SAXException])
  def startAttribute(namespaceURI: String, localName: String, qName: String): Unit

  /**
   * receives notification of the end of an attribute.
   */
  @throws(classOf[SAXException])
  def endAttribute(namespaceURI: String, localName: String, qName: String, `type`: AttributeExp): Unit

  /**
   * this method is called after the start/endAttribute method are called
   * for all attributes.
   */
  @throws(classOf[SAXException])
  def endAttributePart(): Unit
