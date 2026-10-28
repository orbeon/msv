package org.orbeon.msv.verifier.jarv

import javax.xml.parsers.SAXParserFactory
import org.orbeon.msv.grammar.Grammar
import org.orbeon.msv.iso_relax.verifier.VerifierConfigurationException
import org.orbeon.msv.reader.GrammarReaderController
import org.orbeon.msv.reader.util.GrammarLoader
import org.xml.sax.InputSource

class TheFactoryImpl(factory: SAXParserFactory) extends FactoryImpl(factory):

  def this() = this {
    val f = SAXParserFactory.newInstance()
    f.setNamespaceAware(true)
    f
  }

  override protected def parse(is: InputSource, controller: GrammarReaderController): Grammar =
    try
      GrammarLoader.loadSchema(is, controller, factory)
    catch
      case e: Exception =>
        throw new VerifierConfigurationException(e)

  protected def parse(source: String, controller: GrammarReaderController): Grammar =
    try
      GrammarLoader.loadSchema(source, controller, factory)
    catch
      case e: Exception =>
        throw new VerifierConfigurationException(e)
