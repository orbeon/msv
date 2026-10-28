package org.orbeon.msv.datatype.xsd.conformance

case class XmlNode(
  name: String,
  attrs: Map[String, String],
  children: List[XmlNode],
  text: String
)

object TestPatternGenerator:

  def parse(patternElement: XmlNode): TestPattern =
    patternElement.name match
      case "combination" =>
        val mode = patternElement.attrs.get("mode") match
          case Some("or") => false
          case _          => true
        val children = patternElement.children.map(parse).toArray
        new FullCombinationPattern(children, mode)

      case "choice" =>
        val children = patternElement.children.map(parse).toArray
        new ChoiceTestPattern(children)

      case "facet" =>
        val name = patternElement.attrs("name")
        val value = patternElement.attrs("value")
        val answer = trimAnswer(patternElement.attrs.getOrElse("answer", ""))
        val expectedFail = patternElement.attrs.get("expectedFail").contains("true")
        val expectedFailReason = patternElement.attrs.getOrElse("expectedFailReason", null)
        new SimpleTestPattern(name, value, answer, expectedFail, expectedFailReason)

      case other =>
        throw new IllegalArgumentException(s"Unknown pattern: $other")

  def trimAnswer(answer: String): String =
    if answer == null then ""
    else
      val sb = new java.lang.StringBuilder()
      for i <- 0 until answer.length do
        val ch = answer.charAt(i)
        if ch == 'o' || ch == '.' then sb.append(ch)
      sb.toString

  def merge(a1: String, a2: String, mergeAnd: Boolean): String =
    if a1 == null then a2
    else if a2 == null then a1
    else
      if a1.length != a2.length then
        throw new AssertionError(s"Lengths of the answers differ: ${a1.length} vs ${a2.length}")
      val sb = new java.lang.StringBuilder(a1.length)
      for i <- 0 until a1.length do
        val c1 = a1.charAt(i)
        val c2 = a2.charAt(i)
        val matches =
          if mergeAnd then c1 == 'o' && c2 == 'o'
          else c1 == 'o' || c2 == 'o'
        sb.append(if matches then 'o' else '.')
      sb.toString
