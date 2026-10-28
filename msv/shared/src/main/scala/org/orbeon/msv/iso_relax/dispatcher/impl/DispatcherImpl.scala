package org.orbeon.msv.iso_relax.dispatcher.impl

import java.util.{Enumeration, Vector => JVector}
import org.orbeon.msv.iso_relax.dispatcher.{Dispatcher, ElementDecl, IslandVerifier, SchemaProvider}
import org.xml.sax.helpers.NamespaceSupport
import org.xml.sax.{Attributes, ContentHandler, DTDHandler, ErrorHandler, Locator, SAXException, XMLReader}
import scala.compiletime.uninitialized

class DispatcherImpl(private val schema: SchemaProvider) extends Dispatcher {
  import DispatcherImpl._

  private var depth: Int = 1
  protected var documentLocator: Locator = uninitialized
  protected val nsMap: NamespaceSupport = new NamespaceSupport()
  protected var errorHandler: ErrorHandler = uninitialized
  private var currentHandler: IslandVerifier = schema.createTopLevelVerifier()
  private var contextStack: Context = uninitialized
  protected val unparsedEntityDecls: JVector[Dispatcher.UnparsedEntityDecl] = new JVector[Dispatcher.UnparsedEntityDecl]()
  protected val notationDecls: JVector[Dispatcher.NotationDecl] = new JVector[Dispatcher.NotationDecl]()

  protected class Transponder extends ContentHandler with DTDHandler {
    def unparsedEntityDecl(name: String, systemId: String, publicId: String, notation: String): Unit = {
      unparsedEntityDecls.add(new Dispatcher.UnparsedEntityDecl(name, systemId, publicId, notation))
    }

    def notationDecl(name: String, systemId: String, publicId: String): Unit = {
      notationDecls.add(new Dispatcher.NotationDecl(name, systemId, publicId))
    }

    def setDocumentLocator(locator: Locator): Unit = {
      documentLocator = locator
      currentHandler.setDocumentLocator(locator)
    }

    @throws(classOf[SAXException])
    def startElement(uri: String, localName: String, qName: String, attributes: Attributes): Unit = {
      currentHandler.startElement(uri, localName, qName, attributes)
      depth += 1
      nsMap.pushContext()
    }

    @throws(classOf[SAXException])
    def endElement(uri: String, localName: String, qName: String): Unit = {
      nsMap.popContext()
      currentHandler.endElement(uri, localName, qName)

      depth -= 1
      if (depth == 0) {
        val e: Enumeration[?] = nsMap.getDeclaredPrefixes
        while (e.hasMoreElements) {
          currentHandler.endPrefixMapping(e.nextElement().asInstanceOf[String])
        }

        val results = currentHandler.endIsland()

        depth = contextStack.depth
        currentHandler = contextStack.handler
        contextStack = contextStack.previous

        currentHandler.endChildIsland(uri, results)
      }
    }

    @throws(classOf[SAXException])
    def characters(ch: Array[Char], start: Int, length: Int): Unit = {
      currentHandler.characters(ch, start, length)
    }

    @throws(classOf[SAXException])
    def ignorableWhitespace(ch: Array[Char], start: Int, length: Int): Unit = {
      currentHandler.ignorableWhitespace(ch, start, length)
    }

    @throws(classOf[SAXException])
    def processingInstruction(target: String, data: String): Unit = {
      currentHandler.processingInstruction(target, data)
    }

    @throws(classOf[SAXException])
    def skippedEntity(name: String): Unit = {
      currentHandler.skippedEntity(name)
    }

    def startDocument(): Unit = ()
    def endDocument(): Unit = ()

    @throws(classOf[SAXException])
    def startPrefixMapping(prefix: String, uri: String): Unit = {
      nsMap.declarePrefix(prefix, uri)
      currentHandler.startPrefixMapping(prefix, uri)
    }

    @throws(classOf[SAXException])
    def endPrefixMapping(prefix: String): Unit = {
      currentHandler.endPrefixMapping(prefix)
    }
  }

  protected val transponder: Transponder = new Transponder()

  if (currentHandler != null) {
    currentHandler.setDispatcher(this)
  }

  def getSchemaProvider: SchemaProvider = schema

  def attachXMLReader(reader: XMLReader): Unit = {
    reader.setContentHandler(transponder)
  }

  @throws(classOf[SAXException])
  def switchVerifier(newVerifier: IslandVerifier): Unit = {
    contextStack = new Context(currentHandler, depth, contextStack)

    currentHandler = newVerifier
    currentHandler.setDispatcher(this)
    currentHandler.setDocumentLocator(documentLocator)
    depth = 0

    val e: Enumeration[?] = nsMap.getDeclaredPrefixes
    while (e.hasMoreElements) {
      val prefix = e.nextElement().asInstanceOf[String]
      currentHandler.startPrefixMapping(prefix, nsMap.getURI(prefix))
    }
  }

  def setErrorHandler(handler: ErrorHandler): Unit = {
    this.errorHandler = handler
  }

  def getErrorHandler: ErrorHandler = errorHandler

  def countUnparsedEntityDecls(): Int = unparsedEntityDecls.size()

  def getUnparsedEntityDecl(index: Int): Dispatcher.UnparsedEntityDecl = unparsedEntityDecls.get(index)

  def countNotationDecls(): Int = notationDecls.size()

  def getNotationDecl(index: Int): Dispatcher.NotationDecl = notationDecls.get(index)
}

object DispatcherImpl {
  protected class Context(val handler: IslandVerifier, val depth: Int, val previous: Context)
}
