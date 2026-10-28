package org.orbeon.msv.reader.util

import java.io.IOException
import javax.xml.parsers.{ParserConfigurationException, SAXParserFactory}
import org.orbeon.msv.grammar.ExpressionPool
import org.orbeon.msv.grammar.Grammar
import org.orbeon.msv.grammar.xmlschema.XMLSchemaGrammar
import org.orbeon.msv.reader.{Controller, GrammarReader, GrammarReaderController}
import org.orbeon.msv.reader.trex.ng.comp.RELAXNGCompReader
import org.orbeon.msv.reader.xmlschema.XMLSchemaReader
import org.orbeon.msv.verifier.regexp.REDocumentDeclaration
import org.orbeon.msv.verifier.regexp.xmlschema.XSREDocDecl
import org.xml.sax.*
import org.xml.sax.helpers.DefaultHandler
import scala.compiletime.uninitialized

class GrammarLoader:

  private var factory: SAXParserFactory = uninitialized
  private var controller: Controller = uninitialized
  private var pool: ExpressionPool = uninitialized
  private var strictCheck: Boolean = false

  def setSAXParserFactory(factory: SAXParserFactory): Unit =
    this.factory = factory

  def getSAXParserFactory(): SAXParserFactory =
    if factory == null then
      factory = SAXParserFactory.newInstance()
      factory.setNamespaceAware(true)
    factory

  def setController(controller: GrammarReaderController): Unit =
    this.controller = new Controller(controller)

  def getController(): Controller =
    if controller == null then
      controller = new Controller(new GrammarReaderController:
        def warning(locs: Array[Locator], errorMessage: String): Unit = ()
        def error(locs: Array[Locator], errorMessage: String, nestedException: Exception): Unit = ()
        def resolveEntity(s: String, p: String): InputSource = null
      )
    controller

  def setPool(pool: ExpressionPool): Unit =
    this.pool = pool

  def getPool(): ExpressionPool =
    if pool == null then
      pool = new ExpressionPool()
    pool

  def setStrictCheck(value: Boolean): Unit =
    strictCheck = value

  def getStrictCheck(): Boolean = strictCheck

  @throws(classOf[SAXException])
  @throws(classOf[ParserConfigurationException])
  @throws(classOf[IOException])
  def parse(source: InputSource): Grammar =
    _loadSchema(source)

  @throws(classOf[SAXException])
  @throws(classOf[ParserConfigurationException])
  @throws(classOf[IOException])
  def parse(url: String): Grammar =
    _loadSchema(url)

  @throws(classOf[SAXException])
  @throws(classOf[ParserConfigurationException])
  @throws(classOf[IOException])
  def parseVGM(url: String): REDocumentDeclaration =
    val g = _loadSchema(url)
    if g == null then null else GrammarLoader.wrapByVGM(g)

  @throws(classOf[SAXException])
  @throws(classOf[ParserConfigurationException])
  @throws(classOf[IOException])
  def parseVGM(source: InputSource): REDocumentDeclaration =
    val g = _loadSchema(source)
    if g == null then null else GrammarLoader.wrapByVGM(g)

  private def _loadSchema(source: Any): Grammar =
    var isDTD = false
    source match
      case s: String =>
        if GrammarLoader.hasDTDextension(s) then isDTD = true
      case is: InputSource =>
        if GrammarLoader.hasDTDextension(is.getSystemId) then isDTD = true
      case _ =>

    if isDTD then
      throw new UnsupportedOperationException("DTD schema loading is not supported")

    val reader = new Array[GrammarReader](1)
    val parser: XMLReader = getSAXParserFactory().newSAXParser().getXMLReader

    parser.setContentHandler(new DefaultHandler:
      private var locator: Locator = uninitialized
      private val prefixes = collection.mutable.ArrayBuffer[(String, String)]()

      override def setDocumentLocator(loc: Locator): Unit =
        locator = loc

      override def startPrefixMapping(prefix: String, uri: String): Unit =
        prefixes += ((prefix, uri))

      override def startElement(namespaceURI: String, localName: String, qName: String, atts: Attributes): Unit =
        val winner: GrammarReader =
          if localName == "schema" || namespaceURI == XMLSchemaReader.XMLSchemaNamespace || namespaceURI == XMLSchemaReader.XMLSchemaNamespace_old then
            new XMLSchemaReader(getController(), getSAXParserFactory(), getPool())
          else
            new RELAXNGCompReader(getController(), getSAXParserFactory(), getPool())

        reader(0) = winner

        winner.setDocumentLocator(locator)
        winner.startDocument()
        for (prefix, uri) <- prefixes do
          winner.startPrefixMapping(prefix, uri)
        winner.startElement(namespaceURI, localName, qName, atts)
        parser.setContentHandler(winner)
    )

    parser.setErrorHandler(getController())
    parser.setEntityResolver(getController())

    source match
      case s: String =>
        parser.parse(s)
      case is: InputSource =>
        parser.parse(is)
      case other =>
        throw new IllegalArgumentException(s"Unsupported source type: ${other.getClass.getName}")

    if getController().hadError then
      null
    else if reader(0) != null then
      reader(0).getResultAsGrammar
    else
      null

object GrammarLoader:

  private def hasDTDextension(name: String): Boolean =
    if name == null then false
    else
      val idx = name.length - 4
      if idx < 0 then false
      else name.substring(idx).equalsIgnoreCase(".dtd")

  def wrapByVGM(g: Grammar): REDocumentDeclaration =
    g match
      case xsg: XMLSchemaGrammar =>
        new XSREDocDecl(xsg)
      case _ =>
        new REDocumentDeclaration(g)

  @throws(classOf[SAXException])
  @throws(classOf[ParserConfigurationException])
  @throws(classOf[IOException])
  def loadVGM(
    url: String,
    controller: GrammarReaderController,
    factory: SAXParserFactory
  ): REDocumentDeclaration =
    val g = loadSchema(url, controller, factory)
    if g != null then wrapByVGM(g) else null

  @throws(classOf[SAXException])
  @throws(classOf[ParserConfigurationException])
  @throws(classOf[IOException])
  def loadVGM(
    source: InputSource,
    controller: GrammarReaderController,
    factory: SAXParserFactory
  ): REDocumentDeclaration =
    val g = loadSchema(source, controller, factory)
    if g != null then wrapByVGM(g) else null

  @throws(classOf[SAXException])
  @throws(classOf[ParserConfigurationException])
  @throws(classOf[IOException])
  def loadVGM(url: String): REDocumentDeclaration =
    try loadVGM(url, new ThrowController(), null)
    catch case e: GrammarLoaderException => throw e.e

  @throws(classOf[SAXException])
  @throws(classOf[ParserConfigurationException])
  @throws(classOf[IOException])
  def loadVGM(source: InputSource): REDocumentDeclaration =
    try loadVGM(source, new ThrowController(), null)
    catch case e: GrammarLoaderException => throw e.e

  @throws(classOf[SAXException])
  @throws(classOf[ParserConfigurationException])
  @throws(classOf[IOException])
  def loadSchema(
    url: String,
    controller: GrammarReaderController,
    factory: SAXParserFactory
  ): Grammar =
    val loader = new GrammarLoader()
    loader.setController(controller)
    loader.setSAXParserFactory(factory)
    loader.parse(url)

  @throws(classOf[SAXException])
  @throws(classOf[ParserConfigurationException])
  @throws(classOf[IOException])
  def loadSchema(
    source: InputSource,
    controller: GrammarReaderController,
    factory: SAXParserFactory
  ): Grammar =
    val loader = new GrammarLoader()
    loader.setController(controller)
    loader.setSAXParserFactory(factory)
    loader.parse(source)

  @throws(classOf[SAXException])
  @throws(classOf[ParserConfigurationException])
  @throws(classOf[IOException])
  def loadSchema(source: String, controller: GrammarReaderController): Grammar =
    val loader = new GrammarLoader()
    loader.setController(controller)
    loader.parse(source)

  @throws(classOf[SAXException])
  @throws(classOf[ParserConfigurationException])
  @throws(classOf[IOException])
  def loadSchema(source: InputSource, controller: GrammarReaderController): Grammar =
    val loader = new GrammarLoader()
    loader.setController(controller)
    loader.parse(source)

  @throws(classOf[SAXException])
  @throws(classOf[ParserConfigurationException])
  @throws(classOf[IOException])
  def loadSchema(url: String): Grammar =
    try loadSchema(url, new ThrowController(), null)
    catch case e: GrammarLoaderException => throw e.e

  @throws(classOf[SAXException])
  @throws(classOf[ParserConfigurationException])
  @throws(classOf[IOException])
  def loadSchema(source: InputSource): Grammar =
    try loadSchema(source, new ThrowController(), null)
    catch case e: GrammarLoaderException => throw e.e

  private class GrammarLoaderException(val e: SAXException) extends RuntimeException(e.getMessage)

  private class ThrowController extends GrammarReaderController:
    def warning(locs: Array[Locator], errorMessage: String): Unit = ()
    def error(locs: Array[Locator], errorMessage: String, nestedException: Exception): Unit =
      if locs != null then
        for loc <- locs do
          if loc != null then
            throw new GrammarLoaderException(new SAXParseException(errorMessage, loc, nestedException))
      throw new GrammarLoaderException(new SAXException(errorMessage, nestedException))
    def resolveEntity(p: String, s: String): InputSource = null
