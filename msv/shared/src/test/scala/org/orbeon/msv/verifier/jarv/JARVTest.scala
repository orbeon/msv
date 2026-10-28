package org.orbeon.msv.verifier.jarv

import utest.*
import org.orbeon.msv.iso_relax.verifier.{Schema, Verifier, VerifierFactory, VerifierHandler}
import org.orbeon.msv.reader.trex.ng.RELAXNGReader
import org.orbeon.msv.reader.xmlschema.XMLSchemaReader
import org.xml.sax.*
import javax.xml.parsers.SAXParserFactory
import java.io.StringReader

object JARVTest extends TestSuite:

  class SimpleErrorHandler extends ErrorHandler:
    var errors: List[String] = Nil
    var warnings: List[String] = Nil
    def warning(e: SAXParseException): Unit = warnings = e.getMessage :: warnings
    def error(e: SAXParseException): Unit = errors = e.getMessage :: errors
    def fatalError(e: SAXParseException): Unit = errors = e.getMessage :: errors

  val tests = Tests {

    test("Const constants") {
      assert(Const.PANIC_MODE_FEATURE == "http://www.sun.com/xmlns/msv/features/panicMode")
    }

    test("TheFactoryImpl features") {
      val factory = new TheFactoryImpl()
      assert(factory.isFeature(Const.PANIC_MODE_FEATURE))
      factory.setFeature(Const.PANIC_MODE_FEATURE, false)
      assert(!factory.isFeature(Const.PANIC_MODE_FEATURE))
      factory.setFeature(Const.PANIC_MODE_FEATURE, true)
      assert(factory.isFeature(Const.PANIC_MODE_FEATURE))
    }

    test("TheFactoryImpl validates XML Schema via verify(InputSource)") {
      val xsd =
        """<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
          |  <xs:element name="note">
          |    <xs:complexType>
          |      <xs:sequence>
          |        <xs:element name="to" type="xs:string"/>
          |        <xs:element name="from" type="xs:string"/>
          |        <xs:element name="count" type="xs:integer"/>
          |      </xs:sequence>
          |    </xs:complexType>
          |  </xs:element>
          |</xs:schema>""".stripMargin

      val validDoc =
        """<note>
          |  <to>Tove</to>
          |  <from>Jani</from>
          |  <count>42</count>
          |</note>""".stripMargin

      val invalidDoc =
        """<note>
          |  <to>Tove</to>
          |  <from>Jani</from>
          |  <count>not-a-number</count>
          |</note>""".stripMargin

      val factory = new TheFactoryImpl()
      val schema: Schema = factory.compileSchema(new InputSource(new StringReader(xsd)))
      val verifier: Verifier = schema.newVerifier()

      val errorHandler1 = new SimpleErrorHandler
      verifier.setErrorHandler(errorHandler1)
      val validResult = verifier.verify(new InputSource(new StringReader(validDoc)))
      assert(validResult)
      assert(errorHandler1.errors.isEmpty)

      val verifier2: Verifier = schema.newVerifier()
      val errorHandler2 = new SimpleErrorHandler
      verifier2.setErrorHandler(errorHandler2)
      val invalidResult = verifier2.verify(new InputSource(new StringReader(invalidDoc)))
      assert(!invalidResult || errorHandler2.errors.nonEmpty)
    }

    test("TheFactoryImpl validates XML Schema via VerifierHandler SAX pipeline") {
      val xsd =
        """<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
          |  <xs:element name="root">
          |    <xs:complexType>
          |      <xs:sequence>
          |        <xs:element name="item" type="xs:string" maxOccurs="2"/>
          |      </xs:sequence>
          |    </xs:complexType>
          |  </xs:element>
          |</xs:schema>""".stripMargin

      val validDoc =
        """<root>
          |  <item>First</item>
          |  <item>Second</item>
          |</root>""".stripMargin

      val invalidDoc =
        """<root>
          |  <item>First</item>
          |  <item>Second</item>
          |  <item>Third - too many</item>
          |</root>""".stripMargin

      val saxFactory = SAXParserFactory.newInstance()
      saxFactory.setNamespaceAware(true)
      val factory = new TheFactoryImpl(saxFactory)
      factory.setFeature(Const.PANIC_MODE_FEATURE, false)

      val is = new InputSource(new StringReader(xsd))
      val schema: Schema = factory.compileSchema(is)

      // Test valid document using VerifierHandler (matching Orbeon Forms MSVValidationProcessor pattern)
      val verifier = schema.newVerifier()
      val errorHandler = new SimpleErrorHandler
      verifier.setErrorHandler(errorHandler)
      val handler: VerifierHandler = verifier.getVerifierHandler

      val parser = saxFactory.newSAXParser().getXMLReader
      parser.setContentHandler(handler)
      parser.setErrorHandler(errorHandler)
      parser.parse(new InputSource(new StringReader(validDoc)))
      assert(handler.isValid)
      assert(errorHandler.errors.isEmpty)

      // Test invalid document
      val verifier2 = schema.newVerifier()
      val errorHandler2 = new SimpleErrorHandler
      verifier2.setErrorHandler(errorHandler2)
      val handler2: VerifierHandler = verifier2.getVerifierHandler

      val parser2 = saxFactory.newSAXParser().getXMLReader
      parser2.setContentHandler(handler2)
      parser2.setErrorHandler(errorHandler2)
      try
        parser2.parse(new InputSource(new StringReader(invalidDoc)))
      catch
        case _: SAXException =>

      assert(!handler2.isValid || errorHandler2.errors.nonEmpty)
    }

    test("TheFactoryImpl validates RELAX NG schema") {
      val rng =
        """<grammar xmlns="http://relaxng.org/ns/structure/1.0">
          |  <start>
          |    <element name="book">
          |      <element name="title"><text/></element>
          |      <element name="author"><text/></element>
          |    </element>
          |  </start>
          |</grammar>""".stripMargin

      val validDoc = "<book><title>Dune</title><author>Frank Herbert</author></book>"
      val invalidDoc = "<book><title>Dune</title><extra/></book>"

      val factory = new TheFactoryImpl()
      val schema: Schema = factory.compileSchema(new InputSource(new StringReader(rng)))

      val verifier = schema.newVerifier()
      val eh = new SimpleErrorHandler
      verifier.setErrorHandler(eh)
      assert(verifier.verify(new InputSource(new StringReader(validDoc))))
      assert(eh.errors.isEmpty)

      val verifier2 = schema.newVerifier()
      val eh2 = new SimpleErrorHandler
      verifier2.setErrorHandler(eh2)
      val res2 = verifier2.verify(new InputSource(new StringReader(invalidDoc)))
      assert(!res2 || eh2.errors.nonEmpty)
    }

    test("TheFactoryImpl with entity resolver") {
      val mainXsd =
        """<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema" targetNamespace="http://example.com/ns" xmlns:tns="http://example.com/ns" elementFormDefault="qualified">
          |  <xs:include schemaLocation="imported.xsd"/>
          |  <xs:element name="order">
          |    <xs:complexType>
          |      <xs:sequence>
          |        <xs:element name="item" type="tns:ItemType"/>
          |      </xs:sequence>
          |    </xs:complexType>
          |  </xs:element>
          |</xs:schema>""".stripMargin

      val importedXsd =
        """<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema" targetNamespace="http://example.com/ns">
          |  <xs:simpleType name="ItemType">
          |    <xs:restriction base="xs:string">
          |      <xs:enumeration value="widget"/>
          |      <xs:enumeration value="gadget"/>
          |    </xs:restriction>
          |  </xs:simpleType>
          |</xs:schema>""".stripMargin

      val factory = new TheFactoryImpl()
      factory.setEntityResolver(new EntityResolver:
        def resolveEntity(publicId: String, systemId: String): InputSource =
          if systemId != null && systemId.endsWith("imported.xsd") then
            val is = new InputSource(new StringReader(importedXsd))
            is.setSystemId(systemId)
            is
          else null
      )

      val mainSource = new InputSource(new StringReader(mainXsd))
      mainSource.setSystemId("http://example.com/main.xsd")
      val schema = factory.compileSchema(mainSource)

      val validInstance =
        """<order xmlns="http://example.com/ns">
          |  <item>widget</item>
          |</order>""".stripMargin

      val invalidInstance =
        """<order xmlns="http://example.com/ns">
          |  <item>invalid-item</item>
          |</order>""".stripMargin

      val v1 = schema.newVerifier()
      val eh1 = new SimpleErrorHandler
      v1.setErrorHandler(eh1)
      assert(v1.verify(new InputSource(new StringReader(validInstance))))

      val v2 = schema.newVerifier()
      val eh2 = new SimpleErrorHandler
      v2.setErrorHandler(eh2)
      assert(!v2.verify(new InputSource(new StringReader(invalidInstance))) || eh2.errors.nonEmpty)
    }

    test("XSFactoryImpl and RELAXNGFactoryImpl direct instantiation") {
      val xsFactory = new XSFactoryImpl()
      val xsd = "<xs:schema xmlns:xs='http://www.w3.org/2001/XMLSchema'><xs:element name='foo' type='xs:string'/></xs:schema>"
      val schema = xsFactory.compileSchema(new InputSource(new StringReader(xsd)))
      assert(schema.newVerifier().verify(new InputSource(new StringReader("<foo>hello</foo>"))))

      val rngFactory = new RELAXNGFactoryImpl()
      val rng = "<element name='bar' xmlns='http://relaxng.org/ns/structure/1.0'><text/></element>"
      val rngSchema = rngFactory.compileSchema(new InputSource(new StringReader(rng)))
      assert(rngSchema.newVerifier().verify(new InputSource(new StringReader("<bar>world</bar>"))))
    }

    test("FactoryLoaderImpl factory resolution") {
      val loader = new FactoryLoaderImpl()
      assert(loader.createFactory(XMLSchemaReader.XMLSchemaNamespace).isInstanceOf[XSFactoryImpl])
      assert(loader.createFactory(RELAXNGReader.RELAXNGNamespace).isInstanceOf[RELAXNGFactoryImpl])
      assert(loader.createFactory("relax").isInstanceOf[TheFactoryImpl])
      assert(loader.createFactory("unknown") == null)

      VerifierFactory.registerLoader(loader)
      val vf = VerifierFactory.newInstance(XMLSchemaReader.XMLSchemaNamespace)
      assert(vf.isInstanceOf[XSFactoryImpl])
    }
  }
