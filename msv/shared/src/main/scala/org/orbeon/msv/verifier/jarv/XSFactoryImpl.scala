package org.orbeon.msv.verifier.jarv

import javax.xml.parsers.SAXParserFactory
import org.orbeon.msv.grammar.Grammar
import org.orbeon.msv.reader.GrammarReaderController
import org.orbeon.msv.reader.xmlschema.XMLSchemaReader
import org.xml.sax.InputSource

class XSFactoryImpl(factory: SAXParserFactory) extends FactoryImpl(factory):

  def this() = this {
    val f = SAXParserFactory.newInstance()
    f.setNamespaceAware(true)
    f
  }

  override protected def parse(is: InputSource, controller: GrammarReaderController): Grammar =
    XMLSchemaReader.parse(is, factory, controller)
