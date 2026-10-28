package org.orbeon.msv.verifier.identity

import org.orbeon.msv.driver.textui.Debug
import org.orbeon.msv.grammar.xmlschema.Field
import org.orbeon.msv.relaxng.datatype.Datatype
import org.xml.sax.{Attributes, SAXException}

/**
 * XPath matcher that tests one field of a key.
 *
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class FieldMatcher(
  val parent: FieldsMatcher,
  val field: Field,
  namespaceURI: String,
  localName: String
) extends PathMatcher(parent.owner, field.paths):

  /**
   * the matched value. If this field is null, then it means
   * nothing is matched yet.
   */
  var value: AnyRef = null

  /**
   * this field is set to non-null if it's found that an element
   * is matched to this XPath.
   */
  protected var elementText: StringBuilder = null

  // test the initial match
  start(namespaceURI, localName)

  @throws(classOf[SAXException])
  override protected[identity] def onElementMatched(namespaceURI: String, localName: String): Unit =
    if Debug.debug then
      println("field match for " + parent.selector.idConst.localName)
    elementText = new StringBuilder()

  @throws(classOf[SAXException])
  override protected[identity] def onAttributeMatched(
    namespaceURI: String,
    localName: String,
    value: String,
    `type`: Datatype
  ): Unit =
    if Debug.debug then
      println("field match for " + parent.selector.idConst.localName)
    setValue(value, `type`)

  @throws(classOf[SAXException])
  protected def startElement(namespaceURI: String, localName: String, attributes: Attributes): Unit =
    if elementText != null then
      elementText = null
    super.startElement(namespaceURI, localName)

  @throws(classOf[SAXException])
  override protected[identity] def endElement(`type`: Datatype): Unit =
    super.endElement(`type`)
    if elementText != null then
      setValue(elementText.toString, `type`)
      elementText = null

  @throws(classOf[SAXException])
  override protected[identity] def characters(buf: Array[Char], start: Int, len: Int): Unit =
    super.characters(buf, start, len)
    if elementText != null then
      elementText.appendAll(buf, start, len)

  @throws(classOf[SAXException])
  private def setValue(lexical: String, `type`: Datatype): Unit =
    if value != null then
      doubleMatchError()
      return

    if `type` == null then
      value = lexical
      if Debug.debug then
        println("no type info available")
    else
      value = `type`.createValue(lexical, owner)

  @throws(classOf[SAXException])
  private def doubleMatchError(): Unit =
    var i = 0
    while i < parent.children.length do
      if parent.children(i) eq this then
        return reportDoubleMatch(i)
      i += 1
    reportDoubleMatch(0)

  private def reportDoubleMatch(i: Int): Unit =
    owner.reportError(
      null,
      IDConstraintChecker.ERR_DOUBLE_MATCH,
      Array(
        parent.selector.idConst.namespaceURI,
        parent.selector.idConst.localName,
        Integer.valueOf(i + 1)
      )
    )
