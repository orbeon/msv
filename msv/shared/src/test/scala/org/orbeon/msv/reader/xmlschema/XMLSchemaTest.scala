package org.orbeon.msv.reader.xmlschema

import utest.*
import org.orbeon.msv.grammar.*
import org.orbeon.msv.grammar.xmlschema.*
import org.orbeon.msv.reader.GrammarReaderController
import org.orbeon.msv.verifier.regexp.{ResidualCalculator, StringToken}
import org.orbeon.msv.datatype.xsd.DatatypeFactory
import org.xml.sax.{InputSource, Locator}
import java.io.StringReader
import javax.xml.parsers.SAXParserFactory

object XMLSchemaTest extends TestSuite:

  class TestController extends GrammarReaderController:
    var errors: List[String] = Nil
    var warnings: List[String] = Nil
    def warning(locs: Array[Locator], errorMessage: String): Unit =
      warnings = errorMessage :: warnings
    def error(locs: Array[Locator], errorMessage: String, nestedException: Exception): Unit =
      errors = errorMessage :: errors
    def resolveEntity(publicId: String, systemId: String): InputSource = null

  private def parseSchema(schemaXml: String, controller: TestController = new TestController()): (XMLSchemaGrammar, TestController) =
    val factory = SAXParserFactory.newInstance()
    factory.setNamespaceAware(true)
    val is = new InputSource(new StringReader(schemaXml))
    val grammar = XMLSchemaReader.parse(is, factory, controller)
    (grammar, controller)

  val tests = Tests {
    test("element and attribute default value support") {
      val xsd =
        """<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema" targetNamespace="http://example.com" elementFormDefault="qualified">
          |  <xs:element name="title" type="xs:string" default="Untitled"/>
          |  <xs:complexType name="book">
          |    <xs:attribute name="edition" type="xs:string" default="1st"/>
          |  </xs:complexType>
          |</xs:schema>""".stripMargin

      val (grammar, controller) = parseSchema(xsd)
      assert(grammar != null)
      assert(controller.errors.isEmpty)

      val schema = grammar.getByNamespace("http://example.com")
      assert(schema != null)

      val titleDecl = schema.elementDecls.get("title")
      assert(titleDecl != null)
      assert(titleDecl.getElementExp.defaultValue == "Untitled")

      val bookType = schema.complexTypes.get("book")
      assert(bookType != null)
      var foundAttDefault = false
      bookType.body.exp.visit(new org.orbeon.msv.grammar.util.ExpressionWalker {
        override def onAttribute(exp: AttributeExp): Unit =
          if exp.getDefaultValue == "1st" then
            foundAttDefault = true
      })
      assert(foundAttDefault)
    }

    test("element with mutually exclusive fixed and default produces error") {
      val xsd =
        """<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema" targetNamespace="http://example.com">
          |  <xs:element name="bad" type="xs:string" fixed="abc" default="xyz"/>
          |</xs:schema>""".stripMargin

      val (_, controller) = parseSchema(xsd)
      assert(controller.errors.nonEmpty)
      assert(controller.errors.exists(e => e.contains("fixed") || e.contains("default")))
    }

    test("attribute with mutually exclusive fixed and default produces error") {
      val xsd =
        """<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema" targetNamespace="http://example.com">
          |  <xs:attribute name="bad" type="xs:string" fixed="abc" default="xyz"/>
          |</xs:schema>""".stripMargin

      val (_, controller) = parseSchema(xsd)
      assert(controller.errors.nonEmpty)
      assert(controller.errors.exists(e => e.contains("fixed") || e.contains("default")))
    }

    test("complexContent with mixed attribute") {
      val xsd =
        """<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema" targetNamespace="http://example.com">
          |  <xs:complexType name="baseType">
          |    <xs:sequence>
          |      <xs:element name="child" type="xs:string"/>
          |    </xs:sequence>
          |  </xs:complexType>
          |  <xs:complexType name="mixedType">
          |    <xs:complexContent mixed="true">
          |      <xs:extension base="baseType">
          |        <xs:sequence>
          |          <xs:element name="extra" type="xs:string"/>
          |        </xs:sequence>
          |      </xs:extension>
          |    </xs:complexContent>
          |  </xs:complexType>
          |</xs:schema>""".stripMargin

      val (grammar, controller) = parseSchema(xsd)
      assert(grammar != null)
      assert(controller.errors.isEmpty)

      val schema = grammar.getByNamespace("http://example.com")
      val mixedType = schema.complexTypes.get("mixedType")
      assert(mixedType != null)
      assert(mixedType.body.exp.isInstanceOf[MixedExp])
    }

    test("ResidualCalculator choice with multiple list children") {
      val pool = new ExpressionPool()
      val strDt = DatatypeFactory.getTypeByName("string")
      val tokenDt = DatatypeFactory.getTypeByName("token")

      // Choice of two lists
      val list1 = pool.createList(pool.createSequence(pool.createData(strDt), pool.createData(strDt)))
      val list2 = pool.createList(pool.createData(tokenDt))
      val choice = pool.createChoice(list1, list2)

      val calc = new ResidualCalculator(pool)
      // A string token with two words: should match list1
      val token = new StringToken(calc, "hello world", null.asInstanceOf[org.orbeon.msv.grammar.IDContextProvider2])
      val res = calc.calcResidual(choice, token)
      assert(res == Expression.epsilon)
    }
  }
