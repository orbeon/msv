package org.orbeon.msv.verifier.jarv

import java.io.IOException
import javax.xml.parsers.SAXParserFactory
import org.orbeon.msv.grammar.Grammar
import org.orbeon.msv.grammar.xmlschema.XMLSchemaGrammar
import org.orbeon.msv.iso_relax.verifier.{Schema, VerifierConfigurationException, VerifierFactory}
import org.orbeon.msv.reader.GrammarReaderController
import org.orbeon.msv.verifier.{IVerifier, Verifier}
import org.orbeon.msv.verifier.identity.IDConstraintChecker
import org.orbeon.msv.verifier.regexp.REDocumentDeclaration
import org.orbeon.msv.verifier.util.ErrorHandlerImpl
import org.xml.sax.*

abstract class FactoryImpl(protected val factory: SAXParserFactory) extends VerifierFactory:

  def this() = this {
    val f = SAXParserFactory.newInstance()
    f.setNamespaceAware(true)
    f
  }

  private var usePanicMode: Boolean = true

  @throws(classOf[SAXNotRecognizedException])
  @throws(classOf[SAXNotSupportedException])
  override def setFeature(feature: String, v: Boolean): Unit =
    if feature == FactoryImpl.PanicModeFeature then
      usePanicMode = v
    else
      super.setFeature(feature, v)

  @throws(classOf[SAXNotRecognizedException])
  @throws(classOf[SAXNotSupportedException])
  override def isFeature(feature: String): Boolean =
    if feature == FactoryImpl.PanicModeFeature then
      usePanicMode
    else
      super.isFeature(feature)

  @throws(classOf[SAXException])
  @throws(classOf[VerifierConfigurationException])
  protected def parse(source: InputSource, controller: GrammarReaderController): Grammar

  @throws(classOf[VerifierConfigurationException])
  @throws(classOf[SAXException])
  override def compileSchema(source: InputSource): Schema =
    try
      val g = parse(source, new ThrowController())
      if g == null then
        throw new VerifierConfigurationException("unable to parse the schema")
      new SchemaImpl(g, factory, usePanicMode)
    catch
      case we: FactoryImpl.WrapperException =>
        throw we.e
      case e: SAXException =>
        throw e
      case e: VerifierConfigurationException =>
        throw e
      case pce: Exception =>
        throw new VerifierConfigurationException(pce)

  private class ThrowController extends GrammarReaderController:
    def warning(locs: Array[Locator], errorMessage: String): Unit = ()
    def error(locs: Array[Locator], errorMessage: String, nestedException: Exception): Unit =
      if locs != null then
        for loc <- locs do
          if loc != null then
            throw new FactoryImpl.WrapperException(new SAXParseException(errorMessage, loc, nestedException))
      throw new FactoryImpl.WrapperException(new SAXException(errorMessage, nestedException))

    def resolveEntity(p: String, s: String): InputSource =
      val resolver = getEntityResolver
      if resolver == null then null
      else resolver.resolveEntity(p, s)

object FactoryImpl:
  private[jarv] final val PanicModeFeature: String = "http://www.sun.com/xmlns/msv/features/panicMode"
  private class WrapperException(val e: SAXException) extends RuntimeException(e.getMessage)

  def createVerifier(g: Grammar): IVerifier =
    g match
      case xsg: XMLSchemaGrammar =>
        new IDConstraintChecker(xsg, new ErrorHandlerImpl())
      case _ =>
        new Verifier(new REDocumentDeclaration(g), new ErrorHandlerImpl())
