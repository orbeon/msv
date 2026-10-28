package org.orbeon.msv.verifier

import org.orbeon.msv.relaxng.datatype.Datatype
import org.xml.sax.{Attributes, ErrorHandler, Locator, SAXException}
import org.xml.sax.helpers.XMLFilterImpl

/**
 * SAX XMLFilter that verifies incoming SAX event stream.
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class VerifierFilter(val verifier: IVerifier) extends XMLFilterImpl with IVerifier:

  def this(documentDecl: DocumentDeclaration, errorHandler: ErrorHandler) =
    this(new Verifier(documentDecl, errorHandler))

  def isValid: Boolean = verifier.isValid
  def getCurrentElementType: AnyRef = verifier.getCurrentElementType
  def getLastCharacterType: Array[Datatype] = verifier.getLastCharacterType
  final def getLocator: Locator = verifier.getLocator
  final override def getErrorHandler: ErrorHandler = verifier.getErrorHandler
  final override def setErrorHandler(handler: ErrorHandler): Unit =
    super.setErrorHandler(handler)
    verifier.setErrorHandler(handler)

  final def setPanicMode(usePanicMode: Boolean): Unit =
    verifier.setPanicMode(usePanicMode)

  override def setDocumentLocator(locator: Locator): Unit =
    verifier.setDocumentLocator(locator)
    super.setDocumentLocator(locator)

  override def startDocument(): Unit =
    verifier.startDocument()
    super.startDocument()

  override def endDocument(): Unit =
    verifier.endDocument()
    super.endDocument()

  override def startPrefixMapping(prefix: String, uri: String): Unit =
    verifier.startPrefixMapping(prefix, uri)
    super.startPrefixMapping(prefix, uri)

  override def endPrefixMapping(prefix: String): Unit =
    verifier.endPrefixMapping(prefix)
    super.endPrefixMapping(prefix)

  override def startElement(namespaceURI: String, localName: String, qName: String, atts: Attributes): Unit =
    verifier.startElement(namespaceURI, localName, qName, atts)
    super.startElement(namespaceURI, localName, qName, atts)

  override def endElement(namespaceURI: String, localName: String, qName: String): Unit =
    verifier.endElement(namespaceURI, localName, qName)
    super.endElement(namespaceURI, localName, qName)

  override def characters(ch: Array[Char], start: Int, length: Int): Unit =
    verifier.characters(ch, start, length)
    super.characters(ch, start, length)

  override def ignorableWhitespace(ch: Array[Char], start: Int, length: Int): Unit =
    verifier.ignorableWhitespace(ch, start, length)
    super.ignorableWhitespace(ch, start, length)

  override def processingInstruction(target: String, data: String): Unit =
    verifier.processingInstruction(target, data)
    super.processingInstruction(target, data)

  override def skippedEntity(name: String): Unit =
    verifier.skippedEntity(name)
    super.skippedEntity(name)
