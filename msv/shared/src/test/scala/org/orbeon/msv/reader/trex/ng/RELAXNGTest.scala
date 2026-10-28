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

  class TestController(entities: Map[String, String] = Map.empty) extends GrammarReaderController:
    var errors: List[String] = Nil
    var warnings: List[String] = Nil
    def warning(locs: Array[Locator], errorMessage: String): Unit =
      warnings = errorMessage :: warnings
    def error(locs: Array[Locator], errorMessage: String, nestedException: Exception): Unit =
      errors = errorMessage :: errors
    def resolveEntity(publicId: String, systemId: String): InputSource =
      entities.find { case (k, _) => systemId != null && systemId.endsWith(k) } match
        case Some((_, content)) =>
          val is = new InputSource(new StringReader(content))
          is.setSystemId(systemId)
          is
        case None => null

  class TestErrorHandler extends ErrorHandler:
    var errors: List[String] = Nil
    var warnings: List[String] = Nil
    def warning(e: SAXParseException): Unit = warnings = e.getMessage :: warnings
    def error(e: SAXParseException): Unit = errors = e.getMessage :: errors
    def fatalError(e: SAXParseException): Unit = errors = e.getMessage :: errors

  private def parseGrammar(
    schemaXml: String,
    controller: TestController = new TestController(),
    systemId: String = "http://example.com/schema.rng"
  ): (Grammar, TestController) =
    val factory = SAXParserFactory.newInstance()
    factory.setNamespaceAware(true)
    val is = new InputSource(new StringReader(schemaXml))
    is.setSystemId(systemId)
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

    test("advanced content model: interleave and repetition") {
      val rng =
        """<grammar xmlns="http://relaxng.org/ns/structure/1.0">
          |  <start>
          |    <element name="profile">
          |      <interleave>
          |        <element name="name"><text/></element>
          |        <element name="email"><text/></element>
          |        <optional>
          |          <element name="phone"><text/></element>
          |        </optional>
          |      </interleave>
          |    </element>
          |  </start>
          |</grammar>""".stripMargin

      val (grammar, controller) = parseGrammar(rng)
      assert(grammar != null)
      assert(controller.errors.isEmpty)

      // Both element orders are valid
      assert(validate(grammar, "<profile><name>Alice</name><email>a@example.com</email></profile>"))
      assert(validate(grammar, "<profile><email>a@example.com</email><name>Alice</name></profile>"))
      assert(validate(grammar, "<profile><phone>555-1234</phone><name>Alice</name><email>a@example.com</email></profile>"))
      assert(validate(grammar, "<profile><name>Alice</name><phone>555-1234</phone><email>a@example.com</email></profile>"))
      assert(validate(grammar, "<profile><name>Alice</name><email>a@example.com</email><phone>555-1234</phone></profile>"))

      // Missing required element
      assert(!validate(grammar, "<profile><name>Alice</name></profile>"))
      // Duplicate element
      assert(!validate(grammar, "<profile><name>Alice</name><email>a@example.com</email><name>Alice2</name></profile>"))
      // Unexpected element
      assert(!validate(grammar, "<profile><name>Alice</name><email>a@example.com</email><extra/></profile>"))
    }

    test("advanced content model: mixed text and elements") {
      val rng =
        """<grammar xmlns="http://relaxng.org/ns/structure/1.0">
          |  <start>
          |    <element name="paragraph">
          |      <mixed>
          |        <zeroOrMore>
          |          <choice>
          |            <element name="bold"><text/></element>
          |            <element name="italic"><text/></element>
          |            <element name="code"><text/></element>
          |          </choice>
          |        </zeroOrMore>
          |      </mixed>
          |    </element>
          |  </start>
          |</grammar>""".stripMargin

      val (grammar, controller) = parseGrammar(rng)
      assert(grammar != null)
      assert(controller.errors.isEmpty)

      assert(validate(grammar, "<paragraph>Plain text only</paragraph>"))
      assert(validate(grammar, "<paragraph>Text with <bold>bold</bold> and <italic>italic</italic> and <code>code</code> elements.</paragraph>"))
      assert(validate(grammar, "<paragraph><bold>Leading bold</bold> followed by text</paragraph>"))

      // Unknown element inside mixed
      assert(!validate(grammar, "<paragraph>Text with <unknown>bad tag</unknown></paragraph>"))
    }

    test("name classes: anyName and nsName with except") {
      val rng =
        """<grammar xmlns="http://relaxng.org/ns/structure/1.0" ns="http://example.com/ns1">
          |  <start>
          |    <element name="root">
          |      <zeroOrMore>
          |        <element>
          |          <anyName>
          |            <except>
          |              <name ns="http://example.com/ns1">forbidden</name>
          |              <name ns="http://example.com/ns1">blocked</name>
          |            </except>
          |          </anyName>
          |          <text/>
          |        </element>
          |      </zeroOrMore>
          |    </element>
          |  </start>
          |</grammar>""".stripMargin

      val (grammar, controller) = parseGrammar(rng)
      assert(grammar != null)
      assert(controller.errors.isEmpty)

      assert(validate(grammar, """<root xmlns="http://example.com/ns1"><allowed>hello</allowed><other xmlns="http://other.com">world</other></root>"""))
      assert(!validate(grammar, """<root xmlns="http://example.com/ns1"><forbidden>no</forbidden></root>"""))
      assert(!validate(grammar, """<root xmlns="http://example.com/ns1"><blocked>no</blocked></root>"""))
    }

    test("multiple definitions: combine with choice and interleave") {
      val choiceRng =
        """<grammar xmlns="http://relaxng.org/ns/structure/1.0">
          |  <start>
          |    <element name="catalog">
          |      <zeroOrMore>
          |        <ref name="Product"/>
          |      </zeroOrMore>
          |    </element>
          |  </start>
          |  <define name="Product" combine="choice">
          |    <element name="book">
          |      <element name="title"><text/></element>
          |    </element>
          |  </define>
          |  <define name="Product" combine="choice">
          |    <element name="album">
          |      <element name="artist"><text/></element>
          |    </element>
          |  </define>
          |</grammar>""".stripMargin

      val (g1, c1) = parseGrammar(choiceRng)
      assert(g1 != null)
      assert(c1.errors.isEmpty)

      assert(validate(g1, "<catalog><book><title>Scala 3</title></book><album><artist>Miles Davis</artist></album></catalog>"))
      assert(!validate(g1, "<catalog><movie><title>Inception</title></movie></catalog>"))

      val interleaveRng =
        """<grammar xmlns="http://relaxng.org/ns/structure/1.0">
          |  <start>
          |    <element name="card">
          |      <ref name="CardFields"/>
          |    </element>
          |  </start>
          |  <define name="CardFields" combine="interleave">
          |    <element name="front"><text/></element>
          |  </define>
          |  <define name="CardFields" combine="interleave">
          |    <element name="back"><text/></element>
          |  </define>
          |</grammar>""".stripMargin

      val (g2, c2) = parseGrammar(interleaveRng)
      assert(g2 != null)
      assert(c2.errors.isEmpty)

      assert(validate(g2, "<card><front>Question</front><back>Answer</back></card>"))
      assert(validate(g2, "<card><back>Answer</back><front>Question</front></card>"))
      assert(!validate(g2, "<card><front>Question only</front></card>"))
    }

    test("modularity: include and externalRef") {
      val includedRng =
        """<grammar xmlns="http://relaxng.org/ns/structure/1.0">
          |  <start>
          |    <element name="baseItem"><text/></element>
          |  </start>
          |  <define name="Details">
          |    <element name="desc"><text/></element>
          |  </define>
          |</grammar>""".stripMargin

      val mainIncludeRng =
        """<grammar xmlns="http://relaxng.org/ns/structure/1.0">
          |  <include href="common-def.rng">
          |    <start>
          |      <element name="customItem">
          |        <ref name="Details"/>
          |      </element>
          |    </start>
          |  </include>
          |</grammar>""".stripMargin

      val c1 = new TestController(Map("common-def.rng" -> includedRng))
      val (g1, _) = parseGrammar(mainIncludeRng, c1)
      assert(g1 != null)
      assert(c1.errors.isEmpty)

      assert(validate(g1, "<customItem><desc>High quality</desc></customItem>"))
      assert(!validate(g1, "<baseItem>Original</baseItem>"))

      val subPatternRng =
        """<element name="subitem" xmlns="http://relaxng.org/ns/structure/1.0">
          |  <attribute name="code"/>
          |</element>""".stripMargin

      val externalRefRng =
        """<element name="container" xmlns="http://relaxng.org/ns/structure/1.0">
          |  <externalRef href="sub-def.rng"/>
          |</element>""".stripMargin

      val c2 = new TestController(Map("sub-def.rng" -> subPatternRng))
      val (g2, _) = parseGrammar(externalRefRng, c2)
      assert(g2 != null)
      assert(c2.errors.isEmpty)

      assert(validate(g2, """<container><subitem code="S1"/></container>"""))
      assert(!validate(g2, """<container><subitem/></container>"""))
      assert(!validate(g2, """<container><other/></container>"""))
    }

    test("data and value patterns: list, facet parameters, and exact values") {
      val rng =
        """<element name="sensorData" xmlns="http://relaxng.org/ns/structure/1.0"
          |         datatypeLibrary="http://www.w3.org/2001/XMLSchema-datatypes">
          |  <element name="coords">
          |    <list>
          |      <data type="double"/>
          |      <data type="double"/>
          |    </list>
          |  </element>
          |  <element name="deviceId">
          |    <data type="string">
          |      <param name="pattern">[A-Z]{3}-[0-9]{4}</param>
          |    </data>
          |  </element>
          |  <element name="status">
          |    <value type="token">ONLINE</value>
          |  </element>
          |</element>""".stripMargin

      val (grammar, controller) = parseGrammar(rng)
      assert(grammar != null)
      assert(controller.errors.isEmpty)

      assert(validate(grammar, "<sensorData><coords>37.77 -122.41</coords><deviceId>DEV-1024</deviceId><status>ONLINE</status></sensorData>"))
      assert(!validate(grammar, "<sensorData><coords>37.77</coords><deviceId>DEV-1024</deviceId><status>ONLINE</status></sensorData>"))
      assert(!validate(grammar, "<sensorData><coords>37.77 -122.41 0.0</coords><deviceId>DEV-1024</deviceId><status>ONLINE</status></sensorData>"))
      assert(!validate(grammar, "<sensorData><coords>37.77 -122.41</coords><deviceId>bad-dev-id</deviceId><status>ONLINE</status></sensorData>"))
      assert(!validate(grammar, "<sensorData><coords>37.77 -122.41</coords><deviceId>DEV-1024</deviceId><status>OFFLINE</status></sensorData>"))
    }

    test("identity and referential integrity: ID and IDREF") {
      val rng =
        """<element name="organization" xmlns="http://relaxng.org/ns/structure/1.0"
          |         datatypeLibrary="http://www.w3.org/2001/XMLSchema-datatypes">
          |  <zeroOrMore>
          |    <element name="member">
          |      <attribute name="id"><data type="ID"/></attribute>
          |      <optional>
          |        <attribute name="manager"><data type="IDREF"/></attribute>
          |      </optional>
          |      <element name="name"><text/></element>
          |    </element>
          |  </zeroOrMore>
          |</element>""".stripMargin

      val (grammar, controller) = parseGrammar(rng)
      assert(grammar != null)
      assert(controller.errors.isEmpty)

      // Valid references
      val validDoc =
        """<organization>
          |  <member id="M1"><name>Director</name></member>
          |  <member id="M2" manager="M1"><name>Engineer</name></member>
          |</organization>""".stripMargin
      assert(validate(grammar, validDoc))

      // Duplicate ID
      val duplicateDoc =
        """<organization>
          |  <member id="M1"><name>Director</name></member>
          |  <member id="M1"><name>Duplicate</name></member>
          |</organization>""".stripMargin
      assert(!validate(grammar, duplicateDoc))

      // Dangling IDREF
      val danglingDoc =
        """<organization>
          |  <member id="M1" manager="M999"><name>Director</name></member>
          |</organization>""".stripMargin
      assert(!validate(grammar, danglingDoc))
    }

    test("DTD compatibility: IDCompatibilityChecker") {
      // 1. Non-simple attribute name (choice of names) with ID type
      val nonSimpleAttRng =
        """<grammar xmlns="http://relaxng.org/ns/structure/1.0"
          |         xmlns:a="http://relaxng.org/ns/compatibility/annotations/1.0"
          |         datatypeLibrary="http://www.w3.org/2001/XMLSchema-datatypes">
          |  <start>
          |    <element name="record">
          |      <attribute>
          |        <choice>
          |          <name>id1</name>
          |          <name>id2</name>
          |        </choice>
          |        <data type="ID"/>
          |      </attribute>
          |    </element>
          |  </start>
          |</grammar>""".stripMargin

      val (g1, c1) = parseCompGrammar(nonSimpleAttRng)
      assert(g1 != null)
      assert(c1.errors.isEmpty)
      assert(!g1.isIDcompatible)
      assert(c1.warnings.exists(w => w.contains("simple name") || w.contains("semantics")))

      // 2. Misplaced ID type directly in element content
      val misplacedIdRng =
        """<grammar xmlns="http://relaxng.org/ns/structure/1.0"
          |         xmlns:a="http://relaxng.org/ns/compatibility/annotations/1.0"
          |         datatypeLibrary="http://www.w3.org/2001/XMLSchema-datatypes">
          |  <start>
          |    <element name="record">
          |      <data type="ID"/>
          |    </element>
          |  </start>
          |</grammar>""".stripMargin

      val (g2, c2) = parseCompGrammar(misplacedIdRng)
      assert(g2 != null)
      assert(c2.errors.isEmpty)
      assert(!g2.isIDcompatible)
      assert(c2.warnings.exists(w => w.contains("attribute pattern") || w.contains("semantics")))
    }
  }
