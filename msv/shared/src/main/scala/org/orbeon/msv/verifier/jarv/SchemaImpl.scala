package org.orbeon.msv.verifier.jarv

import javax.xml.parsers.{ParserConfigurationException, SAXParserFactory}
import org.orbeon.msv.grammar.Grammar
import org.orbeon.msv.iso_relax.verifier.{Schema, Verifier, VerifierConfigurationException}
import org.orbeon.msv.verifier.IVerifier
import org.xml.sax.{SAXException, XMLReader}

class SchemaImpl(
  protected val grammar: Grammar,
  protected val factory: SAXParserFactory,
  private val usePanicMode: Boolean
) extends Schema:

  def this(grammar: Grammar) =
    this(
      grammar,
      {
        val f = SAXParserFactory.newInstance()
        f.setNamespaceAware(true)
        f
      },
      false
    )

  @throws(classOf[VerifierConfigurationException])
  override def newVerifier(): Verifier =
    val core: IVerifier = FactoryImpl.createVerifier(grammar)
    core.setPanicMode(usePanicMode)
    new VerifierImpl(core, createXMLReader())

  private def createXMLReader(): XMLReader = synchronized {
    try
      factory.newSAXParser().getXMLReader
    catch
      case e: SAXException =>
        throw new VerifierConfigurationException(e)
      case e: ParserConfigurationException =>
        throw new VerifierConfigurationException(e)
  }
