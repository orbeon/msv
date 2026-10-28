package org.orbeon.msv.reader.trex.ng

import utest.*
import org.orbeon.msv.grammar.*
import org.orbeon.msv.grammar.relaxng.RELAXNGGrammar
import org.orbeon.msv.reader.GrammarReaderController
import org.orbeon.msv.reader.trex.ng.comp.RELAXNGCompReader
import org.orbeon.msv.verifier.Verifier
import org.orbeon.msv.verifier.regexp.REDocumentDeclaration
import org.xml.sax.{ErrorHandler, InputSource, Locator, SAXException, SAXParseException}
import java.io.StringReader
import javax.xml.parsers.SAXParserFactory

object RELAXNGTest extends TestSuite:

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

  private def parseGrammar(
    schemaXml: String,
    controller: TestController = new TestController()
  ): (Grammar, TestController) =
    val factory = SAXParserFactory.newInstance()
    factory.setNamespaceAware(true)
    val is = new InputSource(new StringReader(schemaXml))
    val grammar = RELAXNGReader.parse(is, factory, controller)
    (grammar, controller)

  private def parseCompGrammar(
    schemaXml: String,
    controller: TestController = new TestController()
  ): (RELAXNGGrammar, TestController) =
    val factory = SAXParserFactory.newInstance()
    factory.setNamespaceAware(true)
    val is = new InputSource(new StringReader(schemaXml))
    val reader = new RELAXNGCompReader(controller, factory, new ExpressionPool())
    reader.parse(is)
    (reader.getResult().asInstanceOf[RELAXNGGrammar], controller)

  private def validate(grammar: Grammar, docXml: String): Boolean =
    val docDecl = new REDocumentDeclaration(grammar)
    val errorHandler = new TestErrorHandler()
    val verifier = new Verifier(docDecl, errorHandler)
    val factory = SAXParserFactory.newInstance()
    factory.setNamespaceAware(true)
    val parser = factory.newSAXParser()
    val xmlReader = parser.getXMLReader
    xmlReader.setContentHandler(verifier)
    xmlReader.setErrorHandler(errorHandler)
    try
      xmlReader.parse(new InputSource(new StringReader(docXml)))
      verifier.isValid && errorHandler.errors.isEmpty
    catch
      case _: SAXException => false

  val tests = Tests {
    test("simple RELAX NG pattern parsing and validation") {
      val rng =
        """<element name="message" xmlns="http://relaxng.org/ns/structure/1.0">
          |  <text/>
        |</element>""".stripMargin

      val (grammar, controller) = parseGrammar(rng)
      assert(grammar != null)
      assert(controller.errors.isEmpty)

      assert(validate(grammar, "<message>Hello world</message>"))
      assert(!validate(grammar, "<note>Hello world</note>"))
      assert(!validate(grammar, "<message><child/></message>"))
    }

    test("RELAX NG with W3C XML Schema datatypes") {
      val rng =
        """<element name="employee" xmlns="http://relaxng.org/ns/structure/1.0"
          |         datatypeLibrary="http://www.w3.org/2001/XMLSchema-datatypes">
          |  <element name="name"><data type="string"/></element>
          |  <element name="age"><data type="integer"/></element>
          |  <element name="active"><data type="boolean"/></element>
          |</element>""".stripMargin

      val (grammar, controller) = parseGrammar(rng)
      assert(grammar != null)
      assert(controller.errors.isEmpty)

      val validDoc =
        """<employee>
          |  <name>John Doe</name>
          |  <age>42</age>
          |  <active>true</active>
          |</employee>""".stripMargin
      assert(validate(grammar, validDoc))

      val badIntDoc =
        """<employee>
          |  <name>John Doe</name>
          |  <age>forty-two</age>
          |  <active>true</active>
          |</employee>""".stripMargin
      assert(!validate(grammar, badIntDoc))

      val badBoolDoc =
        """<employee>
          |  <name>John Doe</name>
          |  <age>42</age>
          |  <active>not-a-bool</active>
          |</employee>""".stripMargin
      assert(!validate(grammar, badBoolDoc))
    }

    test("attributes, optional, and choice") {
      val rng =
        """<element name="order" xmlns="http://relaxng.org/ns/structure/1.0">
          |  <attribute name="id"/>
          |  <optional>
          |    <attribute name="priority"/>
          |  </optional>
          |  <choice>
          |    <element name="standard"><empty/></element>
          |    <element name="express"><empty/></element>
          |  </choice>
          |</element>""".stripMargin

      val (grammar, controller) = parseGrammar(rng)
      assert(grammar != null)
      assert(controller.errors.isEmpty)

      assert(validate(grammar, """<order id="101"><standard/></order>"""))
      assert(validate(grammar, """<order id="101" priority="high"><express/></order>"""))
      assert(!validate(grammar, """<order><standard/></order>"""))
      assert(!validate(grammar, """<order id="101"/>"""))
      assert(!validate(grammar, """<order id="101"><standard/><express/></order>"""))
    }

    test("grammar with definitions, references, and recursion/repetition") {
      val rng =
        """<grammar xmlns="http://relaxng.org/ns/structure/1.0">
          |  <start>
          |    <ref name="BookPattern"/>
          |  </start>
          |  <define name="BookPattern">
          |    <element name="book">
          |      <element name="title"><text/></element>
          |      <oneOrMore>
          |        <ref name="AuthorPattern"/>
          |      </oneOrMore>
          |    </element>
          |  </define>
          |  <define name="AuthorPattern">
          |    <element name="author">
          |      <text/>
          |    </element>
          |  </define>
          |</grammar>""".stripMargin

      val (grammar, controller) = parseGrammar(rng)
      assert(grammar != null)
      assert(controller.errors.isEmpty)

      assert(validate(grammar, "<book><title>Scala 3</title><author>Martin</author></book>"))
      assert(validate(grammar, "<book><title>Scala 3</title><author>Martin</author><author>Adriaan</author></book>"))
      assert(!validate(grammar, "<book><title>Scala 3</title></book>"))
    }

    test("restriction checker catches invalid RELAX NG constructs") {
      val badStart =
        """<grammar xmlns="http://relaxng.org/ns/structure/1.0">
          |  <start>
          |    <attribute name="illegal"/>
          |  </start>
          |</grammar>""".stripMargin
      val (_, c1) = parseGrammar(badStart)
      assert(c1.errors.nonEmpty)

      val badList =
        """<element name="badList" xmlns="http://relaxng.org/ns/structure/1.0">
          |  <list>
          |    <element name="inner"><empty/></element>
          |  </list>
          |</element>""".stripMargin
      val (_, c2) = parseGrammar(badList)
      assert(c2.errors.nonEmpty)
    }

    test("DTD compatibility annotations with RELAXNGCompReader") {
      val validCompRng =
        """<grammar xmlns="http://relaxng.org/ns/structure/1.0"
          |         xmlns:a="http://relaxng.org/ns/compatibility/annotations/1.0"
          |         datatypeLibrary="http://www.w3.org/2001/XMLSchema-datatypes">
          |  <start>
          |    <element name="item">
          |      <optional>
          |        <attribute name="status" a:defaultValue="pending">
          |          <data type="token"/>
          |        </attribute>
          |      </optional>
          |    </element>
          |  </start>
          |</grammar>""".stripMargin

      val (g1, c1) = parseCompGrammar(validCompRng)
      assert(g1 != null)
      assert(c1.errors.isEmpty)
      assert(g1.isDefaultAttributeValueCompatible)

      val nonOptionalDefValRng =
        """<grammar xmlns="http://relaxng.org/ns/structure/1.0"
          |         xmlns:a="http://relaxng.org/ns/compatibility/annotations/1.0">
          |  <start>
          |    <element name="item">
          |      <attribute name="status" a:defaultValue="pending">
          |        <text/>
          |      </attribute>
          |    </element>
          |  </start>
          |</grammar>""".stripMargin

      val (g2, c2) = parseCompGrammar(nonOptionalDefValRng)
      assert(g2 != null)
      // Non-optional attribute with defaultValue is incompatible
      assert(!g2.isDefaultAttributeValueCompatible || c2.warnings.nonEmpty)
    }
  }
