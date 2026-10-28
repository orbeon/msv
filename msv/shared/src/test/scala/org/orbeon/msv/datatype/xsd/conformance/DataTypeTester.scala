package org.orbeon.msv.datatype.xsd.conformance

import org.orbeon.msv.datatype.xsd.{DatatypeFactory, TypeIncubator, XSDatatype, XSDatatypeImpl}
import org.orbeon.msv.relaxng.datatype.DatatypeException
import org.xml.sax.InputSource

import java.io.StringReader
import javax.xml.parsers.SAXParserFactory
import scala.collection.mutable.{ArrayDeque, ListBuffer}

class DataTypeTester:

  case class ValueCase(text: String, expectedFail: Boolean, expectedFailReason: String)

  val errors: ListBuffer[String] = ListBuffer()
  var totalCasesTested: Long = 0
  var totalCombinationsTested: Long = 0

  def parseXml(xml: String): XmlNode =
    val factory = SAXParserFactory.newInstance()
    factory.setNamespaceAware(true)
    val parser = factory.newSAXParser()

    class Handler extends org.xml.sax.helpers.DefaultHandler:
      private case class Frame(name: String, attrs: Map[String, String], children: ArrayDeque[XmlNode], textBuf: java.lang.StringBuilder)
      private val stack = ArrayDeque[Frame]()
      var root: XmlNode = null

      override def startElement(uri: String, localName: String, qName: String, attributes: org.xml.sax.Attributes): Unit =
        val tag = if localName != null && localName.nonEmpty then localName else qName
        val map = (0 until attributes.getLength).map(i => attributes.getQName(i) -> attributes.getValue(i)).toMap
        stack.append(Frame(tag, map, ArrayDeque(), new java.lang.StringBuilder()))

      override def characters(ch: Array[Char], start: Int, length: Int): Unit =
        if stack.nonEmpty then stack.last.textBuf.append(ch, start, length)

      override def endElement(uri: String, localName: String, qName: String): Unit =
        val frame = stack.removeLast()
        val node = XmlNode(frame.name, frame.attrs, frame.children.toList, frame.textBuf.toString)
        if stack.nonEmpty then
          stack.last.children.append(node)
        else
          root = node

    val handler = new Handler()
    parser.parse(new InputSource(new StringReader(xml)), handler)
    handler.root

  def runSuite(xml: String): Unit =
    val rootNode = parseXml(xml)
    val cases = rootNode.children.filter(_.name == "case")
    for c <- cases do
      runCase(c)

  def runCase(caseNode: XmlNode): Unit =
    val caseName = caseNode.attrs.getOrElse("name", "unnamed")

    // Parse values
    val values = caseNode.children.filter(_.name == "value").map { vNode =>
      ValueCase(
        text = vNode.text,
        expectedFail = vNode.attrs.get("expectedFail").contains("true"),
        expectedFailReason = vNode.attrs.getOrElse("expectedFailReason", null)
      )
    }

    // Parse wrongs
    val wrongs = caseNode.children.find(_.name == "wrongs").map { wNode =>
      wNode.children.filter(_.name == "value").map(_.text)
    }.getOrElse(Nil)

    // Parse facets pattern
    val facetsNode = caseNode.children.find(_.name == "facets").getOrElse {
      throw new IllegalArgumentException(s"No <facets> in case $caseName")
    }
    val pattern = TestPatternGenerator.parse(facetsNode.children.head)

    // Parse answers
    val answers = caseNode.children.filter(_.name == "answer")
    for ansNode <- answers do
      val typeName = ansNode.attrs("for")
      val baseAnswer = TestPatternGenerator.trimAnswer(ansNode.text)
      val baseType = DatatypeFactory.getTypeByName(typeName).asInstanceOf[XSDatatypeImpl]
      if baseType == null then
        errors += s"Undefined type: $typeName in case $caseName"
      else
        testDataType(caseName, baseType, values, wrongs, baseAnswer, pattern)

  private def testDataType(
    caseName: String,
    baseType: XSDatatypeImpl,
    values: List[ValueCase],
    wrongs: List[String],
    baseAnswer: String,
    pattern: TestPattern
  ): Unit =
    pattern.reset()
    while pattern.hasMore do
      val ti = new TypeIncubator(baseType)
      val answer =
        try
          val patAns = pattern.get(ti)
          TestPatternGenerator.merge(baseAnswer, patAns, true)
        catch
          case _: DatatypeException =>
            pattern.next()
            null

      if answer != null then
        val typeObj: XSDatatype =
          try
            ti.derive("anonymousURI", "anonymousLocal")
          catch
            case de: DatatypeException =>
              errors += s"Test case error deriving type ${baseType.getName}: ${de.getMessage}"
              null

        if typeObj != null then
          if answer.length != values.length then
            throw new IllegalStateException(s"Answer length ${answer.length} != values length ${values.length} for ${baseType.getName}")

          // Test each value
          for i <- values.indices do
            totalCasesTested += 1
            val valCase = values(i)
            val testVal = valCase.text
            val expectedValid = answer.charAt(i) == 'o'
            val expectedFailure = valCase.expectedFail || pattern.isExpectedFailure
            val expectedFailReason = if valCase.expectedFail then valCase.expectedFailReason else pattern.getExpectedFailureReason

            val v = typeObj.isValid(testVal, DummyContextProvider)
            val d =
              try
                typeObj.checkValid(testVal, DummyContextProvider)
                true
              catch
                case _: DatatypeException => false

            val o = typeObj.createValue(testVal, DummyContextProvider)
            val jo = typeObj.createJavaObject(testVal, DummyContextProvider)

            var roundTripError = false

            // Check lexical round-trip
            if o != null then
              try
                val s = typeObj.convertToLexicalValue(o, DummyContextProvider)
                val o2 = typeObj.createValue(s, DummyContextProvider)
                if o2 == null || !o.equals(o2) then
                  roundTripError = true
              catch
                case _: UnsupportedOperationException => // ignore
                case _: IllegalArgumentException      => roundTripError = true

            // Check Java object round-trip
            if jo != null then
              val s = typeObj.serializeJavaObject(jo, DummyContextProvider)
              if s == null then
                roundTripError = true
              else
                val jo2 = typeObj.createJavaObject(s, DummyContextProvider)
                if jo2 == null then
                  roundTripError = true

            val consistent = !roundTripError && (o != null) == v && (jo != null) == v
            val matchesPrediction = (v && d && expectedValid) || (!v && !d && !expectedValid)

            if !(consistent && matchesPrediction) then
              if !expectedFailure then
                errors += s"[$caseName] Type=${baseType.getName} value='$testVal' expected=$expectedValid, isValid=$v, checkValid=$d, createValue=${o!=null}, createJavaObject=${jo!=null}, roundTripErr=$roundTripError"

          // Test wrong values
          for wrongVal <- wrongs do
            var wrongAccepted = false
            if typeObj.isValid(wrongVal, DummyContextProvider) then
              wrongAccepted = true
            try
              typeObj.checkValid(wrongVal, DummyContextProvider)
              wrongAccepted = true
            catch
              case _: DatatypeException => // expected

            if typeObj.createJavaObject(wrongVal, DummyContextProvider) != null then
              wrongAccepted = true

            if wrongAccepted then
              errors += s"[$caseName] Wrong value accepted for type ${baseType.getName}: '$wrongVal'"

          totalCombinationsTested += 1

      pattern.next()
