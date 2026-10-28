package org.orbeon.msv.verifier

import java.util.{Map as JMap, Set as JSet}
import org.orbeon.msv.grammar.IDContextProvider2
import org.orbeon.msv.relaxng.datatype.Datatype
import org.orbeon.msv.verifier.regexp.StringToken
import org.xml.sax.{Attributes, ContentHandler, DTDHandler, Locator, SAXException}
import org.xml.sax.helpers.{LocatorImpl, NamespaceSupport}

/**
 * Base implementation for various Verifier implementations.
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
abstract class AbstractVerifier extends ContentHandler with DTDHandler with IDContextProvider2:

  protected var locator: Locator = AbstractVerifier.dummyLocator
  final def getLocator: Locator = locator

  protected var performIDcheck: Boolean = true

  protected val ids: JMap[String, StringToken] = new java.util.HashMap[String, StringToken]()
  protected val idrefs: JSet[String] = new java.util.HashSet[String]()

  def setDocumentLocator(loc: Locator): Unit =
    this.locator = loc

  def skippedEntity(p: String): Unit = ()
  def processingInstruction(name: String, data: String): Unit = ()

  private var contextPushed: Boolean = false

  def startPrefixMapping(prefix: String, uri: String): Unit =
    if !contextPushed then
      namespaceSupport.pushContext()
      contextPushed = true
    namespaceSupport.declarePrefix(prefix, uri)

  def endPrefixMapping(prefix: String): Unit = ()

  @throws(classOf[SAXException])
  def startElement(namespaceUri: String, localName: String, qName: String, atts: Attributes): Unit =
    if !contextPushed then
      namespaceSupport.pushContext()
    contextPushed = false

  @throws(classOf[SAXException])
  def endElement(namespaceUri: String, localName: String, qName: String): Unit =
    namespaceSupport.popContext()

  protected def init(): Unit =
    ids.clear()
    idrefs.clear()

  def notationDecl(name: String, publicId: String, systemId: String): Unit =
    notations.add(name)

  def unparsedEntityDecl(name: String, publicId: String, systemId: String, notationName: String): Unit =
    unparsedEntities.add(name)

  protected val namespaceSupport: NamespaceSupport = new NamespaceSupport()
  private val unparsedEntities: JSet[String] = new java.util.HashSet[String]()
  private val notations: JSet[String] = new java.util.HashSet[String]()

  def resolveNamespacePrefix(prefix: String): String =
    val uri = namespaceSupport.getURI(prefix)
    if uri == null && prefix.length == 0 then ""
    else uri

  def isUnparsedEntity(entityName: String): Boolean =
    unparsedEntities.contains(entityName)

  def isNotation(notationName: String): Boolean =
    notations.contains(notationName)

  def getBaseUri: String = null

  protected def onDuplicateId(id: String): Unit

  def onID(dt: Datatype, token: StringToken): Unit =
    if !performIDcheck then return

    val idType = dt.getIdType()
    if idType == Datatype.ID_TYPE_ID then
      val literal = token.literal.trim
      val existing = ids.get(literal)
      if existing == null then
        ids.put(literal, token)
      else if existing ne token then
        onDuplicateId(literal)
      return

    if idType == Datatype.ID_TYPE_IDREF then
      idrefs.add(token.literal.trim)
      return

    if idType == Datatype.ID_TYPE_IDREFS then
      val tokens = token.literal.split("\\s+").filter(_.nonEmpty)
      var i = 0
      while i < tokens.length do
        idrefs.add(tokens(i))
        i += 1
      return

    throw new Error()

object AbstractVerifier:
  protected val dummyLocator: Locator = new LocatorImpl()
