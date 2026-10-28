package org.orbeon.msv.reader.xmlschema

import utest.*
import org.orbeon.msv.grammar.xmlschema.*
import org.orbeon.msv.reader.GrammarReaderController
import org.orbeon.msv.verifier.identity.IDConstraintChecker
import org.xml.sax.{ErrorHandler, InputSource, Locator, SAXException, SAXParseException}
import java.io.StringReader
import javax.xml.parsers.SAXParserFactory

object XMLSchemaValidationTest extends TestSuite:

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

  private def parseSchema(
    schemaXml: String,
    entities: Map[String, String] = Map.empty
  ): (XMLSchemaGrammar, TestController) =
    val controller = new TestController(entities)
    val factory = SAXParserFactory.newInstance()
    factory.setNamespaceAware(true)
    val is = new InputSource(new StringReader(schemaXml))
    is.setSystemId("http://example.com/schema.xsd")
    val grammar = XMLSchemaReader.parse(is, factory, controller)
    (grammar, controller)

  private def validate(grammar: XMLSchemaGrammar, docXml: String): Boolean =
    val errorHandler = new TestErrorHandler()
    val verifier = new IDConstraintChecker(grammar, errorHandler)
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

    test("content model: sequence with minOccurs and maxOccurs") {
      val xsd =
        """<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
          |  <xs:element name="team">
          |    <xs:complexType>
          |      <xs:sequence>
          |        <xs:element name="leader" type="xs:string"/>
          |        <xs:element name="member" type="xs:string" minOccurs="1" maxOccurs="3"/>
          |      </xs:sequence>
          |    </xs:complexType>
          |  </xs:element>
          |</xs:schema>""".stripMargin

      val (grammar, controller) = parseSchema(xsd)
      assert(grammar != null)
      assert(controller.errors.isEmpty)

      // Valid: 1 leader, 1 member
      assert(validate(grammar, "<team><leader>Alice</leader><member>Bob</member></team>"))
      // Valid: 1 leader, 3 members
      assert(validate(grammar, "<team><leader>Alice</leader><member>Bob</member><member>Charlie</member><member>Dave</member></team>"))
      // Invalid: 0 members (minOccurs=1 violated)
      assert(!validate(grammar, "<team><leader>Alice</leader></team>"))
      // Invalid: 4 members (maxOccurs=3 violated)
      assert(!validate(grammar, "<team><leader>Alice</leader><member>1</member><member>2</member><member>3</member><member>4</member></team>"))
      // Invalid: member before leader (wrong order in sequence)
      assert(!validate(grammar, "<team><member>Bob</member><leader>Alice</leader></team>"))
    }

    test("content model: choice and all groups") {
      val xsd =
        """<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
          |  <xs:element name="notification">
          |    <xs:complexType>
          |      <xs:choice>
          |        <xs:element name="email" type="xs:string"/>
          |        <xs:element name="sms" type="xs:string"/>
          |      </xs:choice>
          |    </xs:complexType>
          |  </xs:element>
          |  <xs:element name="profile">
          |    <xs:complexType>
          |      <xs:all>
          |        <xs:element name="firstName" type="xs:string"/>
          |        <xs:element name="lastName" type="xs:string"/>
          |      </xs:all>
          |    </xs:complexType>
          |  </xs:element>
          |</xs:schema>""".stripMargin

      val (grammar, controller) = parseSchema(xsd)
      assert(grammar != null)
      assert(controller.errors.isEmpty)

      // Choice: either email or sms
      assert(validate(grammar, "<notification><email>user@example.com</email></notification>"))
      assert(validate(grammar, "<notification><sms>+123456789</sms></notification>"))
      assert(!validate(grammar, "<notification><email>a</email><sms>b</sms></notification>"))
      assert(!validate(grammar, "<notification/>"))

      // All: firstName and lastName in any order
      assert(validate(grammar, "<profile><firstName>John</firstName><lastName>Doe</lastName></profile>"))
      assert(validate(grammar, "<profile><lastName>Doe</lastName><firstName>John</firstName></profile>"))
      assert(!validate(grammar, "<profile><firstName>John</firstName></profile>"))
    }

    test("simple type facets: range, pattern, and enumeration") {
      val xsd =
        """<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
          |  <xs:simpleType name="Score">
          |    <xs:restriction base="xs:integer">
          |      <xs:minInclusive value="0"/>
          |      <xs:maxInclusive value="100"/>
          |    </xs:restriction>
          |  </xs:simpleType>
          |  <xs:simpleType name="Code">
          |    <xs:restriction base="xs:string">
          |      <xs:pattern value="[A-Z]{3}-[0-9]{3}"/>
          |    </xs:restriction>
          |  </xs:simpleType>
          |  <xs:simpleType name="Color">
          |    <xs:restriction base="xs:string">
          |      <xs:enumeration value="red"/>
          |      <xs:enumeration value="green"/>
          |      <xs:enumeration value="blue"/>
          |    </xs:restriction>
          |  </xs:simpleType>
          |  <xs:element name="test">
          |    <xs:complexType>
          |      <xs:sequence>
          |        <xs:element name="score" type="Score"/>
          |        <xs:element name="code" type="Code"/>
          |        <xs:element name="color" type="Color"/>
          |      </xs:sequence>
          |    </xs:complexType>
          |  </xs:element>
          |</xs:schema>""".stripMargin

      val (grammar, controller) = parseSchema(xsd)
      assert(grammar != null)
      assert(controller.errors.isEmpty)

      val validDoc = "<test><score>85</score><code>ABC-123</code><color>green</color></test>"
      assert(validate(grammar, validDoc))

      // Out-of-range score
      assert(!validate(grammar, "<test><score>105</score><code>ABC-123</code><color>green</color></test>"))
      assert(!validate(grammar, "<test><score>-1</score><code>ABC-123</code><color>green</color></test>"))
      // Invalid pattern
      assert(!validate(grammar, "<test><score>50</score><code>ab-123</code><color>green</color></test>"))
      // Invalid enumeration value
      assert(!validate(grammar, "<test><score>50</score><code>ABC-123</code><color>yellow</color></test>"))
    }

    test("complex type extension") {
      val xsd =
        """<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
          |  <xs:complexType name="Person">
          |    <xs:sequence>
          |      <xs:element name="name" type="xs:string"/>
          |    </xs:sequence>
          |  </xs:complexType>
          |  <xs:complexType name="Employee">
          |    <xs:complexContent>
          |      <xs:extension base="Person">
          |        <xs:sequence>
          |          <xs:element name="department" type="xs:string"/>
          |        </xs:sequence>
          |        <xs:attribute name="id" type="xs:string" use="required"/>
          |      </xs:extension>
          |    </xs:complexContent>
          |  </xs:complexType>
          |  <xs:element name="emp" type="Employee"/>
          |</xs:schema>""".stripMargin

      val (grammar, controller) = parseSchema(xsd)
      assert(grammar != null)
      assert(controller.errors.isEmpty)

      assert(validate(grammar, """<emp id="E100"><name>Alice</name><department>Engineering</department></emp>"""))
      // Missing inherited name
      assert(!validate(grammar, """<emp id="E100"><department>Engineering</department></emp>"""))
      // Missing required attribute
      assert(!validate(grammar, """<emp><name>Alice</name><department>Engineering</department></emp>"""))
    }

    test("substitution groups") {
      val xsd =
        """<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema" elementFormDefault="qualified">
          |  <xs:element name="animal" type="xs:string" abstract="true"/>
          |  <xs:element name="cat" type="xs:string" substitutionGroup="animal"/>
          |  <xs:element name="dog" type="xs:string" substitutionGroup="animal"/>
          |  <xs:element name="zoo">
          |    <xs:complexType>
          |      <xs:sequence>
          |        <xs:element ref="animal" maxOccurs="unbounded"/>
          |      </xs:sequence>
          |    </xs:complexType>
          |  </xs:element>
          |</xs:schema>""".stripMargin

      val (grammar, controller) = parseSchema(xsd)
      assert(grammar != null)
      assert(controller.errors.isEmpty)

      // Valid: cat and dog substituting animal
      assert(validate(grammar, "<zoo><cat>Whiskers</cat><dog>Rover</dog><cat>Felix</cat></zoo>"))
      // Invalid: abstract animal cannot be used directly
      assert(!validate(grammar, "<zoo><animal>Generic</animal></zoo>"))
      // Invalid: unknown element
      assert(!validate(grammar, "<zoo><bird>Tweety</bird></zoo>"))
    }

    test("wildcards: xs:any with skip and strict processContents") {
      val xsd =
        """<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
          |  <xs:element name="wrapper">
          |    <xs:complexType>
          |      <xs:sequence>
          |        <xs:element name="header" type="xs:string"/>
          |        <xs:any processContents="skip" minOccurs="0" maxOccurs="unbounded"/>
          |      </xs:sequence>
          |    </xs:complexType>
          |  </xs:element>
          |</xs:schema>""".stripMargin

      val (grammar, controller) = parseSchema(xsd)
      assert(grammar != null)
      assert(controller.errors.isEmpty)

      // skip allows arbitrary well-formed elements
      assert(validate(grammar, "<wrapper><header>Title</header><custom1/><custom2>data</custom2></wrapper>"))
      assert(validate(grammar, "<wrapper><header>Title</header></wrapper>"))
      // missing required header
      assert(!validate(grammar, "<wrapper><custom1/></wrapper>"))
    }

    test("xsi:type dynamic type substitution") {
      val xsd =
        """<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema"
          |           xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
          |  <xs:complexType name="Shape">
          |    <xs:sequence/>
          |  </xs:complexType>
          |  <xs:complexType name="Circle">
          |    <xs:complexContent>
          |      <xs:extension base="Shape">
          |        <xs:sequence>
          |          <xs:element name="radius" type="xs:double"/>
          |        </xs:sequence>
          |      </xs:extension>
          |    </xs:complexContent>
          |  </xs:complexType>
          |  <xs:element name="figure" type="Shape"/>
          |</xs:schema>""".stripMargin

      val (grammar, controller) = parseSchema(xsd)
      assert(grammar != null)
      assert(controller.errors.isEmpty)

      // Dynamic substitution with Circle
      val validCircle =
        """<figure xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="Circle">
          |  <radius>5.5</radius>
          |</figure>""".stripMargin
      assert(validate(grammar, validCircle))

      // Invalid Circle (missing radius)
      val invalidCircle =
        """<figure xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="Circle"/>"""
      assert(!validate(grammar, invalidCircle))
    }

    test("xsi:nil nillable element support") {
      val xsd =
        """<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
          |  <xs:element name="entry">
          |    <xs:complexType>
          |      <xs:sequence>
          |        <xs:element name="optionalDate" type="xs:date" nillable="true"/>
          |        <xs:element name="mandatoryDate" type="xs:date" nillable="false"/>
          |      </xs:sequence>
          |    </xs:complexType>
          |  </xs:element>
          |</xs:schema>""".stripMargin

      val (grammar, controller) = parseSchema(xsd)
      assert(grammar != null)
      assert(controller.errors.isEmpty)

      // Valid: optionalDate is nil, mandatoryDate has date
      val validNilDoc =
        """<entry xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
          |  <optionalDate xsi:nil="true"/>
          |  <mandatoryDate>2026-10-28</mandatoryDate>
          |</entry>""".stripMargin
      assert(validate(grammar, validNilDoc))

      // Valid: optionalDate not nil with date
      val validDateDoc =
        """<entry xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
          |  <optionalDate>2026-01-01</optionalDate>
          |  <mandatoryDate>2026-10-28</mandatoryDate>
          |</entry>""".stripMargin
      assert(validate(grammar, validDateDoc))

      // Invalid: mandatoryDate has xsi:nil="true" but is not nillable
      val invalidNilDoc =
        """<entry xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
          |  <optionalDate xsi:nil="true"/>
          |  <mandatoryDate xsi:nil="true"/>
          |</entry>""".stripMargin
      assert(!validate(grammar, invalidNilDoc))

      // Invalid: nillable element with content
      val invalidContentWithNil =
        """<entry xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
          |  <optionalDate xsi:nil="true">2026-01-01</optionalDate>
          |  <mandatoryDate>2026-10-28</mandatoryDate>
          |</entry>""".stripMargin
      assert(!validate(grammar, invalidContentWithNil))
    }

    test("identity constraints: xs:key and xs:keyref") {
      val xsd =
        """<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
          |  <xs:element name="company">
          |    <xs:complexType>
          |      <xs:sequence>
          |        <xs:element name="dept" maxOccurs="unbounded">
          |          <xs:complexType>
          |            <xs:sequence>
          |              <xs:element name="name" type="xs:string"/>
          |            </xs:sequence>
          |            <xs:attribute name="id" type="xs:string" use="required"/>
          |          </xs:complexType>
          |        </xs:element>
          |        <xs:element name="emp" maxOccurs="unbounded">
          |          <xs:complexType>
          |            <xs:sequence>
          |              <xs:element name="name" type="xs:string"/>
          |            </xs:sequence>
          |            <xs:attribute name="deptRef" type="xs:string" use="required"/>
          |          </xs:complexType>
          |        </xs:element>
          |      </xs:sequence>
          |    </xs:complexType>
          |    <xs:key name="deptKey">
          |      <xs:selector xpath="dept"/>
          |      <xs:field xpath="@id"/>
          |    </xs:key>
          |    <xs:keyref name="deptKeyRef" refer="deptKey">
          |      <xs:selector xpath="emp"/>
          |      <xs:field xpath="@deptRef"/>
          |    </xs:keyref>
          |  </xs:element>
          |</xs:schema>""".stripMargin

      val (grammar, controller) = parseSchema(xsd)
      assert(grammar != null)
      assert(controller.errors.isEmpty)

      // Valid: unique dept keys, keyref refers to existing key
      val validDoc =
        """<company>
          |  <dept id="D1"><name>Engineering</name></dept>
          |  <dept id="D2"><name>Marketing</name></dept>
          |  <emp deptRef="D1"><name>Alice</name></emp>
          |  <emp deptRef="D2"><name>Bob</name></emp>
          |</company>""".stripMargin
      assert(validate(grammar, validDoc))

      // Invalid: duplicate dept id "D1"
      val duplicateKeyDoc =
        """<company>
          |  <dept id="D1"><name>Engineering</name></dept>
          |  <dept id="D1"><name>QA</name></dept>
          |  <emp deptRef="D1"><name>Alice</name></emp>
          |</company>""".stripMargin
      assert(!validate(grammar, duplicateKeyDoc))

      // Invalid: emp references non-existent dept "D99"
      val danglingKeyRefDoc =
        """<company>
          |  <dept id="D1"><name>Engineering</name></dept>
          |  <emp deptRef="D99"><name>Alice</name></emp>
          |</company>""".stripMargin
      assert(!validate(grammar, danglingKeyRefDoc))
    }

    test("multi-schema composition: xs:include") {
      val commonXsd =
        """<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
          |  <xs:simpleType name="NonEmptyString">
          |    <xs:restriction base="xs:string">
          |      <xs:minLength value="1"/>
          |    </xs:restriction>
          |  </xs:simpleType>
          |</xs:schema>""".stripMargin

      val mainXsd =
        """<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
          |  <xs:include schemaLocation="common.xsd"/>
          |  <xs:element name="item">
          |    <xs:complexType>
          |      <xs:sequence>
          |        <xs:element name="title" type="NonEmptyString"/>
          |      </xs:sequence>
          |    </xs:complexType>
          |  </xs:element>
          |</xs:schema>""".stripMargin

      val (grammar, controller) = parseSchema(mainXsd, Map("common.xsd" -> commonXsd))
      assert(grammar != null)
      assert(controller.errors.isEmpty)

      assert(validate(grammar, "<item><title>Valid Title</title></item>"))
      assert(!validate(grammar, "<item><title></title></item>"))
    }
  }
