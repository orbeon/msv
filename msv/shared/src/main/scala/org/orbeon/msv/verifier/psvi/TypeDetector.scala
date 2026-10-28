package org.orbeon.msv.verifier.psvi

import scala.compiletime.uninitialized
import org.orbeon.msv.grammar.ElementExp
import org.orbeon.msv.relaxng.datatype.Datatype
import org.orbeon.msv.util.{DatatypeRef, StartTagInfo, StringRef}
import org.orbeon.msv.verifier.{Acceptor, DocumentDeclaration, ErrorInfo, ValidityViolation, Verifier}
import org.orbeon.msv.verifier.regexp.{ComplexAcceptor, REDocumentDeclaration, SimpleAcceptor}
import org.xml.sax.{Attributes, ErrorHandler, Locator, SAXException}

/**
 * assign types to the incoming SAX2 events and reports them to
 * the application handler through TypedContentHandler.
 *
 * @author <a href="mailto:kohsuke.kawaguchi@sun.com">Kohsuke KAWAGUCHI</a>
 */
class TypeDetector(documentDecl: DocumentDeclaration, errorHandler: ErrorHandler)
  extends Verifier(documentDecl, errorHandler):

  /** characters that were read (but not processed) */
  private var text: StringBuilder = new StringBuilder()

  protected var handler: TypedContentHandler = uninitialized

  def this(documentDecl: DocumentDeclaration, handler: TypedContentHandler, errorHandler: ErrorHandler) =
    this(documentDecl, errorHandler)
    setContentHandler(handler)

  /**
   * sets the TypedContentHandler which will received the type-augmented
   * infoset.
   */
  def setContentHandler(handler: TypedContentHandler): Unit =
    this.handler = handler

  private val characterType: DatatypeRef = new DatatypeRef()

  @throws(classOf[SAXException])
  override protected def verifyText(): Unit =
    if text.length != 0 then
      val txt = text.toString
      if !current.onText2(txt, this, null, characterType) then
        // error
        // diagnose error, if possible
        val err = new StringRef()
        current.onText2(txt, this, err, null)

        // report an error
        errorHandler.error(
          new ValidityViolation(
            locator,
            Verifier.localizeMessage(Verifier.ERR_UNEXPECTED_TEXT, null),
            new ErrorInfo.BadText(txt)
          )
        )

      // characters are validated. report to the handler.
      reportCharacterChunks(txt, characterType.types)
      text = new StringBuilder()

  @throws(classOf[SAXException])
  private def reportCharacterChunks(text: String, types: Array[Datatype]): Unit =
    if types == null then
      // unable to assign type.
      throw new TypeDetector.AmbiguousDocumentException(getLocator)

    types.length match
      case 0 =>
      case 1 =>
        handler.characterChunk(text, types(0))
      case _ =>
        val tokens = text.trim.split("\\s+").filter(_.nonEmpty)
        var i = 0
        while i < types.length do
          handler.characterChunk(tokens(i), types(i))
          i += 1
        if tokens.length > types.length then
          throw new Error()

  @throws(classOf[SAXException])
  override protected def feedAttribute(
    child: Acceptor,
    uri: String,
    localName: String,
    qName: String,
    value: String
  ): Array[Datatype] =
    handler.startAttribute(uri, localName, qName)
    val result = super.feedAttribute(child, uri, localName, qName, value)
    reportCharacterChunks(value, result)
    handler.endAttribute(
      uri,
      localName,
      qName,
      docDecl.asInstanceOf[REDocumentDeclaration].attToken.matchedExp
    )
    result

  @throws(classOf[SAXException])
  override def startElement(namespaceUri: String, localName: String, qName: String, atts: Attributes): Unit =
    super.startElement(namespaceUri, localName, qName, atts)
    handler.endAttributePart()

  @throws(classOf[SAXException])
  override protected def onNextAcceptorReady(sti: StartTagInfo, nextAcceptor: Acceptor): Unit =
    handler.startElement(sti.namespaceURI, sti.localName, sti.qName)

  @throws(classOf[SAXException])
  override def endElement(namespaceUri: String, localName: String, qName: String): Unit =
    val child = current
    super.endElement(namespaceUri, localName, qName)

    val `type`: ElementExp = child match
      case sa: SimpleAcceptor =>
        sa.owner
      case ca: ComplexAcceptor =>
        val exps = ca.getSatisfiedOwners()
        if exps.length != 1 then
          throw new TypeDetector.AmbiguousDocumentException(getLocator)
        exps(0)
      case _ =>
        throw new Error()

    handler.endElement(namespaceUri, localName, qName, `type`)

  @throws(classOf[SAXException])
  override def characters(buf: Array[Char], start: Int, len: Int): Unit =
    text.appendAll(buf, start, len)

  @throws(classOf[SAXException])
  override def ignorableWhitespace(buf: Array[Char], start: Int, len: Int): Unit =
    text.appendAll(buf, start, len)

  @throws(classOf[SAXException])
  override def startDocument(): Unit =
    super.startDocument()
    handler.startDocument(this)

  @throws(classOf[SAXException])
  override def endDocument(): Unit =
    super.endDocument()
    handler.endDocument()

object TypeDetector:
  class AmbiguousDocumentException(val location: Locator) extends SAXException(""):
    def this() = this(null)
    def getLocation: Locator = location
