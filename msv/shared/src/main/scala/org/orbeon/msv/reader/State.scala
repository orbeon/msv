package org.orbeon.msv.reader

import org.xml.sax.{ContentHandler, Locator, SAXException}
import org.xml.sax.helpers.LocatorImpl
import org.orbeon.msv.grammar.Expression
import org.orbeon.msv.util.StartTagInfo

/**
 * base interface of 'parsing state'.
 * 
 * parsing of XML representation of a grammar is done by
 * using various states.
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
abstract class State extends ContentHandler:

  protected var parentState: State = scala.compiletime.uninitialized
  final def getParentState: State = parentState

  var reader: GrammarReader = scala.compiletime.uninitialized

  protected var startTag: StartTagInfo = scala.compiletime.uninitialized
  def getStartTag: StartTagInfo = startTag

  protected var location: Locator = scala.compiletime.uninitialized
  def getLocation: Locator = location

  protected var baseURI: String = scala.compiletime.uninitialized
  def getBaseURI: String = baseURI

  protected[reader] final def init(reader: GrammarReader, parentState: State, startTag: StartTagInfo): Unit =
    this.reader = reader
    this.parentState = parentState
    this.startTag = startTag
    if reader.locator != null then
      this.location = new LocatorImpl(reader.locator)

    var base: String = null
    if startTag != null then
      base = startTag.getAttribute("http://www.w3.org/XML/1998/namespace", "base")

    if parentState == null then
      this.baseURI = null
    else
      this.baseURI = parentState.baseURI
      if this.baseURI == null && reader.locator != null then
        this.baseURI = reader.locator.getSystemId

    if base != null then
      this.baseURI = reader.combineURI(this.baseURI, base)
    startSelf()

  protected def startSelf(): Unit = ()

  @throws(classOf[SAXException])
  def characters(buffer: Array[Char], from: Int, len: Int): Unit =
    var i = from
    while i < from + len do
      buffer(i) match
        case ' ' | '\t' | '\n' | '\r' =>
        case _ =>
          reader.reportError(GrammarReader.ERR_CHARACTERS, new String(buffer, from, len).trim)
          return
      i += 1

  protected final def callInterceptExpression(exp: Expression): Expression =
    reader.interceptExpression(this, exp)

  @throws(classOf[SAXException])
  def processingInstruction(target: String, data: String): Unit = ()

  @throws(classOf[SAXException])
  def ignorableWhitespace(buffer: Array[Char], from: Int, len: Int): Unit = ()

  @throws(classOf[SAXException])
  def skippedEntity(name: String): Unit = ()

  @throws(classOf[SAXException])
  def endDocument(): Unit = ()

  @throws(classOf[SAXException])
  final def startDocument(): Unit = ()

  def setDocumentLocator(loc: Locator): Unit = ()

  @throws(classOf[SAXException])
  def startPrefixMapping(prefix: String, uri: String): Unit = ()

  @throws(classOf[SAXException])
  def endPrefixMapping(prefix: String): Unit = ()

object State:
  def _assert(b: Boolean): Unit =
    if !b then throw new InternalError()
