package org.orbeon.msv.datatype.xsd.conformance

import utest.*

object DataTypeConformanceTest extends TestSuite:

  private lazy val (tester, caseMap) =
    val t = new DataTypeTester()
    val root = t.parseXml(DataTypeTestData.dataTypeTestXml)
    val map = root.children.filter(_.name == "case").map(c => c.attrs.getOrElse("name", "") -> c).toMap
    (t, map)

  private def runCase(name: String): Unit =
    val c = caseMap.getOrElse(name, throw new NoSuchElementException(s"Case not found: $name"))
    val t = new DataTypeTester()
    t.runCase(c)
    if t.errors.nonEmpty then
      val sample = t.errors.take(10).mkString("\n  ")
      throw new java.lang.AssertionError(s"${t.errors.size} error(s) in $name:\n  $sample")

  val tests = Tests {
    test("QName") - runCase("test for QName")
    test("anyURI") - runCase("test for anyURI")
    test("gDay") - runCase("test for day")
    test("anyURI(2)") - runCase("test for anyURI(2)")
    test("dateTime") - runCase("test for dateTime")
    test("duration") - runCase("simple test for duration")
    test("time") - runCase("test for time")
    test("gMonth") - runCase("test for gMonth")
    test("Name, NCName, NMTOKEN") - runCase("test for Name,NCName,NMTOKEN")
    test("hexBinary") - runCase("test for hexBinary")
    test("base64Binary") - runCase("test for base64")
    test("language") - runCase("test for language")
    test("token") - runCase("test for token")
    test("normalizedString") - runCase("test for normalizedString")
    test("string") - runCase("test for string")
    test("gYear") - runCase("test for year")
    test("date") - runCase("test for date")
    test("boolean") - runCase("test for boolean")
    test("integer and derived types") - runCase("test for integer and its derived types")
    test("float") - runCase("test for float (selected edge cases)")
    test("double") - runCase("test for double (selected edge cases)")
  }
