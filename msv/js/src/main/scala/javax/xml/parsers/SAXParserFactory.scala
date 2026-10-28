package javax.xml.parsers

import org.xml.sax.XMLReader

abstract class SAXParserFactory {
  private var namespaceAware = false
  private var validating = false

  def newSAXParser(): SAXParser

  def setNamespaceAware(awareness: Boolean): Unit = {
    this.namespaceAware = awareness
  }

  def setValidating(validating: Boolean): Unit = {
    this.validating = validating
  }

  def isNamespaceAware: Boolean = namespaceAware
  def isValidating: Boolean = validating
}

class SAXParserFactoryImpl extends SAXParserFactory {
  def newSAXParser(): SAXParser = new SAXParser {
    private val saxParser = new org.orbeon.apache.xerces.parsers.SAXParser(
      new org.orbeon.apache.xerces.parsers.NonValidatingConfiguration()
    )
    if (isNamespaceAware) {
      saxParser.setFeature("http://xml.org/sax/features/namespaces", true)
      saxParser.setFeature("http://xml.org/sax/features/namespace-prefixes", false)
    }
    def getXMLReader: XMLReader = saxParser
  }
}

object SAXParserFactory {
  def newInstance(): SAXParserFactory = new SAXParserFactoryImpl()
}
