package javax.xml.parsers

import org.xml.sax.{InputSource, XMLReader}
import org.xml.sax.helpers.DefaultHandler

abstract class SAXParser {
  def getXMLReader: XMLReader

  def parse(is: InputSource, dh: DefaultHandler): Unit = {
    val reader = getXMLReader
    reader.setContentHandler(dh)
    reader.setErrorHandler(dh)
    reader.setEntityResolver(dh)
    reader.setDTDHandler(dh)
    reader.parse(is)
  }
}
