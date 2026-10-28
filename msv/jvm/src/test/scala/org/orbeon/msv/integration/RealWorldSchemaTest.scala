package org.orbeon.msv.integration

import java.io.{File, FileInputStream, StringReader}
import javax.xml.parsers.SAXParserFactory
import org.orbeon.msv.grammar.Grammar
import org.orbeon.msv.grammar.xmlschema.XMLSchemaGrammar
import org.orbeon.msv.reader.GrammarReaderController
import org.orbeon.msv.reader.trex.ng.RELAXNGReader
import org.orbeon.msv.reader.xmlschema.XMLSchemaReader
import org.orbeon.msv.verifier.Verifier
import org.orbeon.msv.verifier.identity.IDConstraintChecker
import org.orbeon.msv.verifier.regexp.REDocumentDeclaration
import org.xml.sax.{ErrorHandler, InputSource, Locator, SAXException, SAXParseException}
import utest.*

object RealWorldSchemaTest extends TestSuite:

  class TestController extends GrammarReaderController:
    var errors: List[String] = Nil
    var warnings: List[String] = Nil
    def warning(locs: Array[Locator], errorMessage: String): Unit =
      warnings = errorMessage :: warnings
    def error(locs: Array[Locator], errorMessage: String, nestedException: Exception): Unit =
      errors = errorMessage :: errors
    def resolveEntity(publicId: String, systemId: String): InputSource = null

  class TestErrorHandler extends ErrorHandler:
    var errors: List[String] = Nil
    var warnings: List[String] = Nil
    def warning(e: SAXParseException): Unit = warnings = e.getMessage :: warnings
    def error(e: SAXParseException): Unit = errors = e.getMessage :: errors
    def fatalError(e: SAXParseException): Unit = errors = e.getMessage :: errors

  private val baseResourceDir: File =
    val candidates = List(
      new File("shared/src/test/resources"),
      new File("msv/shared/src/test/resources"),
      new File("../shared/src/test/resources")
    )
    candidates.find(_.exists()).getOrElse(new File("shared/src/test/resources"))

  private def parseXMLSchema(schemaFile: File): (XMLSchemaGrammar, TestController) =
    val controller = new TestController()
    val factory = SAXParserFactory.newInstance()
    factory.setNamespaceAware(true)
    val is = new InputSource(new FileInputStream(schemaFile))
    is.setSystemId(schemaFile.toURI.toString)
    val grammar = XMLSchemaReader.parse(is, factory, controller)
    (grammar, controller)

  private def validateXMLSchema(grammar: XMLSchemaGrammar, docFile: File): (Boolean, List[String]) =
    val errorHandler = new TestErrorHandler()
    val verifier = new IDConstraintChecker(grammar, errorHandler)
    val factory = SAXParserFactory.newInstance()
    factory.setNamespaceAware(true)
    val parser = factory.newSAXParser()
    val xmlReader = parser.getXMLReader
    xmlReader.setContentHandler(verifier)
    xmlReader.setErrorHandler(errorHandler)
    val is = new InputSource(new FileInputStream(docFile))
    is.setSystemId(docFile.toURI.toString)
    try
      xmlReader.parse(is)
      (verifier.isValid && errorHandler.errors.isEmpty, errorHandler.errors)
    catch
      case e: SAXException => (false, e.getMessage :: errorHandler.errors)

  private def validateXMLSchemaString(grammar: XMLSchemaGrammar, xmlContent: String): (Boolean, List[String]) =
    val errorHandler = new TestErrorHandler()
    val verifier = new IDConstraintChecker(grammar, errorHandler)
    val factory = SAXParserFactory.newInstance()
    factory.setNamespaceAware(true)
    val parser = factory.newSAXParser()
    val xmlReader = parser.getXMLReader
    xmlReader.setContentHandler(verifier)
    xmlReader.setErrorHandler(errorHandler)
    val is = new InputSource(new StringReader(xmlContent))
    is.setSystemId("http://example.com/doc.xml")
    try
      xmlReader.parse(is)
      (verifier.isValid && errorHandler.errors.isEmpty, errorHandler.errors)
    catch
      case e: SAXException => (false, e.getMessage :: errorHandler.errors)

  private def parseRELAXNG(schemaFile: File): (Grammar, TestController) =
    val controller = new TestController()
    val factory = SAXParserFactory.newInstance()
    factory.setNamespaceAware(true)
    val is = new InputSource(new FileInputStream(schemaFile))
    is.setSystemId(schemaFile.toURI.toString)
    val grammar = RELAXNGReader.parse(is, factory, controller)
    (grammar, controller)

  private def validateRELAXNG(grammar: Grammar, docFile: File): (Boolean, List[String]) =
    val errorHandler = new TestErrorHandler()
    val verifier = new Verifier(new REDocumentDeclaration(grammar), errorHandler)
    val factory = SAXParserFactory.newInstance()
    factory.setNamespaceAware(true)
    val parser = factory.newSAXParser()
    val xmlReader = parser.getXMLReader
    xmlReader.setContentHandler(verifier)
    xmlReader.setErrorHandler(errorHandler)
    val is = new InputSource(new FileInputStream(docFile))
    is.setSystemId(docFile.toURI.toString)
    try
      xmlReader.parse(is)
      (verifier.isValid && errorHandler.errors.isEmpty, errorHandler.errors)
    catch
      case e: SAXException => (false, e.getMessage :: errorHandler.errors)

  val tests = Tests {

    test("UBL 2.3 Invoice validation") {
      val schemaFile = new File(baseResourceDir, "grammar/ubl2.3/xsd/maindoc/UBL-Invoice-2.3.xsd")
      val docFile = new File(baseResourceDir, "files/ubl2.3/UBL-Invoice-2.1-Example.xml")

      assert(schemaFile.exists(), docFile.exists())
      val (grammar, controller) = parseXMLSchema(schemaFile)
      assert(grammar != null)
      assert(controller.errors.isEmpty)

      // Valid document
      val (valid, errors) = validateXMLSchema(grammar, docFile)
      assert(valid, errors.isEmpty)

      // Invalid document: remove mandatory IssueDate
      val xmlContent = java.nio.file.Files.readString(docFile.toPath)
      val mutatedXml = xmlContent.replaceFirst("<cbc:IssueDate>[^<]+</cbc:IssueDate>", "")
      val (invalidValid, invalidErrors) = validateXMLSchemaString(grammar, mutatedXml)
      assert(!invalidValid, invalidErrors.nonEmpty)
    }

    test("UBL 2.3 CreditNote validation") {
      val schemaFile = new File(baseResourceDir, "grammar/ubl2.3/xsd/maindoc/UBL-CreditNote-2.3.xsd")
      val docFile = new File(baseResourceDir, "files/ubl2.3/UBL-CreditNote-2.1-Example.xml")

      assert(schemaFile.exists(), docFile.exists())
      val (grammar, controller) = parseXMLSchema(schemaFile)
      assert(grammar != null)
      assert(controller.errors.isEmpty)

      val (valid, errors) = validateXMLSchema(grammar, docFile)
      assert(valid, errors.isEmpty)
    }

    test("UBL 2.3 Order validation") {
      val schemaFile = new File(baseResourceDir, "grammar/ubl2.3/xsd/maindoc/UBL-Order-2.3.xsd")
      val docFile = new File(baseResourceDir, "files/ubl2.3/UBL-Order-2.1-Example.xml")

      assert(schemaFile.exists(), docFile.exists())
      val (grammar, controller) = parseXMLSchema(schemaFile)
      assert(grammar != null)
      assert(controller.errors.isEmpty)

      val (valid, errors) = validateXMLSchema(grammar, docFile)
      assert(valid, errors.isEmpty)
    }

    test("UN/CEFACT CII validation") {
      val schemaFile = new File(baseResourceDir, "grammar/D16B SCRDM (Subset)/coupled clm/CII/uncefact/data/standard/CrossIndustryInvoice_100pD16B.xsd")
      val docFile1 = new File(baseResourceDir, "files/D16B SCRDM (Subset)/CII_example1.xml")
      val docFile2 = new File(baseResourceDir, "files/cii-multi-prefix.xml")

      assert(schemaFile.exists(), docFile1.exists(), docFile2.exists())
      val (grammar, controller) = parseXMLSchema(schemaFile)
      assert(grammar != null)
      assert(controller.errors.isEmpty)

      val (valid1, errors1) = validateXMLSchema(grammar, docFile1)
      assert(valid1, errors1.isEmpty)

      // Invalid document: insert illegal unexpected element
      val ciiXml = java.nio.file.Files.readString(docFile1.toPath)
      val mutatedCii = ciiXml.replaceFirst("<rsm:SupplyChainTradeTransaction>", "<rsm:UnexpectedTradeTransaction/><rsm:SupplyChainTradeTransaction>")
      val (invalidCii, invalidCiiErrors) = validateXMLSchemaString(grammar, mutatedCii)
      assert(!invalidCii, invalidCiiErrors.nonEmpty)

      val (valid2, errors2) = validateXMLSchema(grammar, docFile2)
      assert(valid2, errors2.isEmpty)
    }

    test("OASIS OpenDocument 1.3 RELAX NG validation") {
      val schemaFile = new File(baseResourceDir, "grammar/odf1.3/OpenDocument-v1.3-schema.rng")
      val validDoc = new File(baseResourceDir, "files/odf1.3/character-styles_odt/content.xml")
      val invalidDoc = new File(baseResourceDir, "files/odf1.3/character-styles_odt/wrong.xml")
      val multiPrefixDoc = new File(baseResourceDir, "files/odf-multi-prefix.xml")

      assert(schemaFile.exists(), validDoc.exists(), invalidDoc.exists(), multiPrefixDoc.exists())
      val (grammar, controller) = parseRELAXNG(schemaFile)
      assert(grammar != null)
      assert(controller.errors.isEmpty)

      val (valid1, errors1) = validateRELAXNG(grammar, validDoc)
      assert(valid1, errors1.isEmpty)

      val (valid2, _) = validateRELAXNG(grammar, invalidDoc)
      assert(!valid2)

      val (valid3, errors3) = validateRELAXNG(grammar, multiPrefixDoc)
      assert(valid3, errors3.isEmpty)
    }
  }
