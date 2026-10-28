package org.orbeon.msv.datatype.xsd

import utest.*
import org.orbeon.msv.datatype.ErrorDatatypeLibrary
import org.orbeon.msv.datatype.xsd.ngimpl.DataTypeLibraryImpl
import org.orbeon.msv.relaxng.datatype.{DatatypeException, ValidationContext}

object XSDatatypeTest extends TestSuite:

  private val dummyContext: ValidationContext = new ValidationContext:
    def isNotation(s: String): Boolean = true
    def isUnparsedEntity(s: String): Boolean = s == "validEntity"
    def resolveNamespacePrefix(ns: String): String =
      if ns == "xs" then "http://www.w3.org/2001/XMLSchema"
      else if ns == "foo" then "http://example.com/foo"
      else if ns == "" then "http://example.com/default"
      else null
    def getBaseUri: String = null

  val tests = Tests {
    test("builtin type lookups from DatatypeFactory") {
      val types = Array(
        "string", "boolean", "decimal", "float", "double", "duration",
        "dateTime", "time", "date", "gYearMonth", "gYear", "gMonthDay",
        "gDay", "gMonth", "hexBinary", "base64Binary", "anyURI", "QName",
        "NOTATION", "normalizedString", "token", "language", "NMTOKEN",
        "NMTOKENS", "Name", "NCName", "ID", "IDREF", "IDREFS", "ENTITY",
        "ENTITIES", "integer", "nonPositiveInteger", "negativeInteger",
        "long", "int", "short", "byte", "nonNegativeInteger",
        "unsignedLong", "unsignedInt", "unsignedShort", "unsignedByte",
        "positiveInteger", "anySimpleType"
      )

      var i = 0
      while i < types.length do
        val typeName = types(i)
        val dt = DatatypeFactory.getTypeByName(typeName)
        assert(dt != null)
        assert(dt.getName == typeName)
        i += 1

      val caught = try { DatatypeFactory.getTypeByName("nonExistentType"); false } catch { case _: DatatypeException => true }
      assert(caught)
    }

    test("boolean datatype validation and value creation") {
      val boolType = DatatypeFactory.getTypeByName("boolean")
      assert(boolType.isValid("true", dummyContext))
      assert(boolType.isValid("false", dummyContext))
      assert(boolType.isValid("1", dummyContext))
      assert(boolType.isValid("0", dummyContext))
      assert(!boolType.isValid("True", dummyContext))
      assert(!boolType.isValid("FALSE", dummyContext))
      assert(!boolType.isValid("2", dummyContext))

      assert(boolType.createValue("true", dummyContext) == java.lang.Boolean.TRUE)
      assert(boolType.createValue("0", dummyContext) == java.lang.Boolean.FALSE)
    }

    test("integer and derived integer validation") {
      val intType = DatatypeFactory.getTypeByName("int")
      assert(intType.isValid("0", dummyContext))
      assert(intType.isValid("+42", dummyContext))
      assert(intType.isValid("-12345", dummyContext))
      assert(intType.isValid("2147483647", dummyContext))
      assert(intType.isValid("-2147483648", dummyContext))
      assert(!intType.isValid("2147483648", dummyContext)) // overflow
      assert(!intType.isValid("-2147483649", dummyContext)) // underflow
      assert(!intType.isValid("abc", dummyContext))
      assert(!intType.isValid("1.5", dummyContext))

      val posInt = DatatypeFactory.getTypeByName("positiveInteger")
      assert(posInt.isValid("1", dummyContext))
      assert(posInt.isValid("999999999999999999999", dummyContext))
      assert(!posInt.isValid("0", dummyContext))
      assert(!posInt.isValid("-1", dummyContext))

      val nonNegInt = DatatypeFactory.getTypeByName("nonNegativeInteger")
      assert(nonNegInt.isValid("0", dummyContext))
      assert(nonNegInt.isValid("1", dummyContext))
      assert(!nonNegInt.isValid("-1", dummyContext))

      val uByte = DatatypeFactory.getTypeByName("unsignedByte")
      assert(uByte.isValid("0", dummyContext))
      assert(uByte.isValid("255", dummyContext))
      assert(!uByte.isValid("256", dummyContext))
      assert(!uByte.isValid("-1", dummyContext))
    }

    test("decimal type validation and comparison") {
      val decimalType = DatatypeFactory.getTypeByName("decimal").asInstanceOf[NumberType]
      assert(decimalType.isValid("0", dummyContext))
      assert(decimalType.isValid("123.456", dummyContext))
      assert(decimalType.isValid("+123.456", dummyContext))
      assert(decimalType.isValid("-0.001", dummyContext))
      assert(decimalType.isValid(".5", dummyContext))
      assert(decimalType.isValid("5.", dummyContext))
      assert(!decimalType.isValid(".", dummyContext))
      assert(!decimalType.isValid("1e5", dummyContext))

      val v1 = decimalType.createValue("123.45", dummyContext)
      val v2 = decimalType.createValue("123.4500", dummyContext)
      assert(decimalType.sameValue(v1, v2))
    }

    test("float and double validation") {
      val floatType = DatatypeFactory.getTypeByName("float")
      assert(floatType.isValid("NaN", dummyContext))
      assert(floatType.isValid("INF", dummyContext))
      assert(floatType.isValid("-INF", dummyContext))
      assert(floatType.isValid("3.14", dummyContext))
      assert(floatType.isValid("-1.2E3", dummyContext))
      assert(!floatType.isValid("3.14f", dummyContext))
      assert(!floatType.isValid("3.14F", dummyContext))

      val doubleType = DatatypeFactory.getTypeByName("double")
      assert(doubleType.isValid("NaN", dummyContext))
      assert(doubleType.isValid("1.23456789e-10", dummyContext))
      assert(!doubleType.isValid("1.23456789f", dummyContext))
      assert(!doubleType.isValid("1.23456789F", dummyContext))
    }

    test("hexBinary and base64Binary validation and conversion") {
      val hex = DatatypeFactory.getTypeByName("hexBinary")
      assert(hex.isValid("0123456789ABCDEFabcdef", dummyContext))
      assert(!hex.isValid("123", dummyContext)) // odd length
      assert(!hex.isValid("ZZ", dummyContext))

      val b64 = DatatypeFactory.getTypeByName("base64Binary")
      assert(b64.isValid("YW55IGNhcm5hbCBwbGVhc3VyZS4=", dummyContext))
      assert(b64.isValid("", dummyContext))
      assert(b64.isValid("YW55 IGNh cm5h bCBw bGVh c3Vy ZS4=", dummyContext)) // whitespace allowed
      assert(b64.isValid(" \n YW55IGNhcm5hbCBwbGVhc3VyZS4= \t ", dummyContext))
      assert(!b64.isValid("YW55IGNhcm5hbCBwbGVhc3VyZS", dummyContext)) // missing padding
      assert(!b64.isValid("YW55IGNhcm5hbCBwbGVhc3VyZS4%", dummyContext)) // illegal character
      assert(!b64.isValid("YW55!GNhcm5hbCBwbGVhc3VyZS4=", dummyContext)) // illegal character
    }

    test("QName validation with ValidationContext") {
      val qname = DatatypeFactory.getTypeByName("QName")
      assert(qname.isValid("xs:string", dummyContext))
      assert(qname.isValid("foo:bar", dummyContext))
      assert(qname.isValid("simple", dummyContext))
      assert(!qname.isValid("unknown:bar", dummyContext)) // unknown prefix
      assert(!qname.isValid("foo:bar:baz", dummyContext))

      val v = qname.createValue("foo:bar", dummyContext).asInstanceOf[QnameValueType]
      assert(v.namespaceURI == "http://example.com/foo")
      assert(v.localPart == "bar")
    }

    test("TypeIncubator facets - length, minLength, maxLength") {
      val strType = DatatypeFactory.getTypeByName("string")
      val ti = new TypeIncubator(strType)
      ti.addFacet("minLength", "3", false, dummyContext)
      ti.addFacet("maxLength", "5", false, dummyContext)
      val restricted = ti.derive("", "shortString")

      assert(restricted.isValid("abc", dummyContext))
      assert(restricted.isValid("abcd", dummyContext))
      assert(restricted.isValid("abcde", dummyContext))
      assert(!restricted.isValid("ab", dummyContext))
      assert(!restricted.isValid("abcdef", dummyContext))

      // Inconsistent facets: minLength > maxLength
      val tiBad = new TypeIncubator(strType)
      tiBad.addFacet("minLength", "10", false, dummyContext)
      tiBad.addFacet("maxLength", "5", false, dummyContext)
      val caught = try { tiBad.derive("", "bad"); false } catch { case _: DatatypeException => true }
      assert(caught)
    }

    test("TypeIncubator facets - pattern facet") {
      val strType = DatatypeFactory.getTypeByName("string")
      val ti = new TypeIncubator(strType)
      ti.addFacet("pattern", "[a-z]+-[0-9]{3}", false, dummyContext)
      val patternType = ti.derive("", "code")

      assert(patternType.isValid("abc-123", dummyContext))
      assert(!patternType.isValid("ABC-123", dummyContext))
      assert(!patternType.isValid("abc-12", dummyContext))
      assert(!patternType.isValid("abc-1234", dummyContext))
    }

    test("TypeIncubator facets - enumeration facet") {
      val intType = DatatypeFactory.getTypeByName("int")
      val ti = new TypeIncubator(intType)
      ti.addFacet("enumeration", "10", false, dummyContext)
      ti.addFacet("enumeration", "20", false, dummyContext)
      ti.addFacet("enumeration", "30", false, dummyContext)
      val enumType = ti.derive("", "tens")

      assert(enumType.isValid("10", dummyContext))
      assert(enumType.isValid("20", dummyContext))
      assert(enumType.isValid("30", dummyContext))
      assert(!enumType.isValid("15", dummyContext))
      assert(!enumType.isValid("0", dummyContext))
    }

    test("TypeIncubator facets - range facets") {
      val intType = DatatypeFactory.getTypeByName("int")
      val ti = new TypeIncubator(intType)
      ti.addFacet("minInclusive", "10", false, dummyContext)
      ti.addFacet("maxExclusive", "20", false, dummyContext)
      val rangeType = ti.derive("", "range10to19")

      assert(rangeType.isValid("10", dummyContext))
      assert(rangeType.isValid("15", dummyContext))
      assert(rangeType.isValid("19", dummyContext))
      assert(!rangeType.isValid("9", dummyContext))
      assert(!rangeType.isValid("20", dummyContext))
    }

    test("TypeIncubator facets - totalDigits and fractionDigits") {
      val decType = DatatypeFactory.getTypeByName("decimal")
      val ti = new TypeIncubator(decType)
      ti.addFacet("totalDigits", "4", false, dummyContext)
      ti.addFacet("fractionDigits", "2", false, dummyContext)
      val curType = ti.derive("", "price")

      assert(curType.isValid("12.34", dummyContext))
      assert(curType.isValid("1.2", dummyContext))
      assert(curType.isValid("99", dummyContext))
      assert(!curType.isValid("123.45", dummyContext)) // 5 digits total
      assert(!curType.isValid("1.234", dummyContext)) // 3 fraction digits
    }

    test("TotalDigitsFacet precision count") {
      assert(TotalDigitsFacet.countPrecision("0") == 1)
      assert(TotalDigitsFacet.countPrecision("100") == 3)
      assert(TotalDigitsFacet.countPrecision("-11100") == 5)
      assert(TotalDigitsFacet.countPrecision("+00123") == 3)
      assert(TotalDigitsFacet.countPrecision("1.20") == 2)
      assert(TotalDigitsFacet.countPrecision("1.2000") == 2)
      assert(TotalDigitsFacet.countPrecision("0.050") == 2)
      assert(TotalDigitsFacet.countPrecision("+00123.4500") == 5)
    }

    test("derivation by list") {
      val intType = DatatypeFactory.getTypeByName("int")
      val listType = DatatypeFactory.deriveByList("", "intList", intType)

      assert(listType.isValid("1 2 3 4 5", dummyContext))
      assert(listType.isValid("  10   20   30  ", dummyContext))
      assert(listType.isValid("", dummyContext))
      assert(!listType.isValid("1 2 abc", dummyContext))

      val lv = listType.createValue("10 20 30", dummyContext).asInstanceOf[ListValueType]
      assert(lv.values.length == 3)
      assert(lv.values(0) == java.lang.Integer.valueOf(10))
      assert(lv.values(1) == java.lang.Integer.valueOf(20))
      assert(lv.values(2) == java.lang.Integer.valueOf(30))
    }

    test("derivation by union") {
      val intType = DatatypeFactory.getTypeByName("int")
      val boolType = DatatypeFactory.getTypeByName("boolean")
      val unionType = DatatypeFactory.deriveByUnion("", "intOrBool", Array(intType, boolType))

      assert(unionType.isValid("123", dummyContext))
      assert(unionType.isValid("true", dummyContext))
      assert(unionType.isValid("false", dummyContext))
      assert(!unionType.isValid("abc", dummyContext))
    }

    test("DataTypeLibraryImpl and ErrorDatatypeLibrary") {
      val libImpl = new DataTypeLibraryImpl()
      val stringDt = libImpl.createDatatype("string")
      assert(stringDt != null)
      assert(stringDt.asInstanceOf[XSDatatype].getName == "string")

      val builder = libImpl.createDatatypeBuilder("token")
      assert(builder != null)

      val errorLib = ErrorDatatypeLibrary.theInstance
      assert(errorLib.createDatatype("anything") != null)
    }
  }
