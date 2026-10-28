package org.orbeon.msv.iso_relax.verifier.impl

import java.io.{File, IOException}
import javax.xml.parsers.{ParserConfigurationException, SAXParserFactory}
import org.orbeon.msv.iso_relax.verifier.{Verifier, VerifierConfigurationException, VerifierFilter, VerifierHandler}
import org.w3c.dom.{Attr, Document, Element, Node}
import org.xml.sax.helpers.{AttributesImpl, LocatorImpl}
import org.xml.sax.{EntityResolver, ErrorHandler, InputSource, SAXException, SAXNotRecognizedException, SAXNotSupportedException, SAXParseException, XMLReader}
import scala.compiletime.uninitialized

abstract class VerifierImpl @throws(classOf[VerifierConfigurationException]) () extends Verifier {
  protected var reader: XMLReader = uninitialized

  protected var errorHandler: ErrorHandler = new ErrorHandler {
    def warning(e: SAXParseException): Unit = ()
    def error(e: SAXParseException): Unit = ()
    def fatalError(e: SAXParseException): Unit = ()
  }

  protected var entityResolver: EntityResolver = uninitialized
  private var filter: VerifierFilter = uninitialized

  prepareXMLReader()

  @throws(classOf[VerifierConfigurationException])
  protected def prepareXMLReader(): Unit = {
    try {
      val factory = SAXParserFactory.newInstance()
      factory.setNamespaceAware(true)
      reader = factory.newSAXParser().getXMLReader
    } catch {
      case e: SAXException =>
        throw new VerifierConfigurationException(e)
      case pce: ParserConfigurationException =>
        throw new VerifierConfigurationException(pce)
    }
  }

  @throws(classOf[SAXNotRecognizedException])
  @throws(classOf[SAXNotSupportedException])
  def isFeature(feature: String): Boolean = {
    if (Verifier.FEATURE_HANDLER == feature || Verifier.FEATURE_FILTER == feature)
      true
    else
      throw new SAXNotRecognizedException(feature)
  }

  @throws(classOf[SAXNotRecognizedException])
  @throws(classOf[SAXNotSupportedException])
  def setFeature(feature: String, value: Boolean): Unit =
    throw new SAXNotRecognizedException(feature)

  @throws(classOf[SAXNotRecognizedException])
  @throws(classOf[SAXNotSupportedException])
  def getProperty(property: String): AnyRef =
    throw new SAXNotRecognizedException(property)

  @throws(classOf[SAXNotRecognizedException])
  @throws(classOf[SAXNotSupportedException])
  def setProperty(property: String, value: AnyRef): Unit =
    throw new SAXNotRecognizedException(property)

  def setErrorHandler(handler: ErrorHandler): Unit =
    this.errorHandler = handler

  def setEntityResolver(resolver: EntityResolver): Unit =
    this.entityResolver = resolver

  @throws(classOf[SAXException])
  @throws(classOf[IOException])
  def verify(uri: String): Boolean =
    verify(new InputSource(uri))

  @throws(classOf[SAXException])
  @throws(classOf[IOException])
  def verify(source: InputSource): Boolean = {
    val handler = getVerifierHandler
    reader.setErrorHandler(errorHandler)
    if (entityResolver != null)
      reader.setEntityResolver(entityResolver)
    reader.setContentHandler(handler)
    reader.parse(source)
    handler.isValid
  }

  @throws(classOf[SAXException])
  @throws(classOf[IOException])
  def verify(f: File): Boolean = {
    var uri = "file:" + f.getAbsolutePath
    if (File.separatorChar == '\\')
      uri = uri.replace('\\', '/')
    verify(new InputSource(uri))
  }

  @throws(classOf[SAXException])
  def verify(node: Node): Boolean = {
    val handler = getVerifierHandler
    handler.setDocumentLocator(new LocatorImpl())
    handler.startDocument()
    VerifierImpl.traverseDom(node, handler)
    handler.endDocument()
    handler.isValid
  }

  @throws(classOf[SAXException])
  def getVerifierFilter: VerifierFilter = {
    if (filter == null)
      filter = new VerifierFilterImpl(this)
    filter
  }
}

object VerifierImpl {
  @throws(classOf[SAXException])
  private def traverseDom(node: Node, handler: org.xml.sax.ContentHandler): Unit = {
    node.getNodeType match {
      case Node.DOCUMENT_NODE =>
        val doc = node.asInstanceOf[Document]
        var child = doc.getFirstChild
        while (child != null) {
          traverseDom(child, handler)
          child = child.getNextSibling
        }

      case Node.ELEMENT_NODE =>
        val elem = node.asInstanceOf[Element]
        val attrs = elem.getAttributes
        val attLen = if (attrs != null) attrs.getLength else 0
        val saxAttrs = new AttributesImpl()
        var i = 0
        while (i < attLen) {
          val a = attrs.item(i).asInstanceOf[Attr]
          var uri = a.getNamespaceURI
          var local = a.getLocalName
          if (uri == null) uri = ""
          if (local == null) local = a.getName
          saxAttrs.addAttribute(uri, local, a.getName, "CDATA", a.getValue)
          i += 1
        }
        var elemUri = elem.getNamespaceURI
        var elemLocal = elem.getLocalName
        if (elemUri == null) elemUri = ""
        if (elemLocal == null) elemLocal = elem.getNodeName
        val qName = elem.getNodeName

        handler.startElement(elemUri, elemLocal, qName, saxAttrs)
        var child = elem.getFirstChild
        while (child != null) {
          traverseDom(child, handler)
          child = child.getNextSibling
        }
        handler.endElement(elemUri, elemLocal, qName)

      case Node.TEXT_NODE | Node.CDATA_SECTION_NODE =>
        val text = node.getNodeValue
        if (text != null) {
          val chars = text.toCharArray
          handler.characters(chars, 0, chars.length)
        }

      case _ =>
    }
  }
}
