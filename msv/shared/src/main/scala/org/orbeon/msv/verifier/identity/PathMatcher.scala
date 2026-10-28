package org.orbeon.msv.verifier.identity

import org.orbeon.msv.grammar.xmlschema.XPath
import org.orbeon.msv.relaxng.datatype.Datatype
import org.xml.sax.SAXException

/**
 * Base implementation of XPath matching engine.
 *
 * It only supports the subset defined in XML Schema Part 1. Extra care
 * must be taken to call the testInitialMatch method after the creation of an object.
 *
 * Match to an attribute is not supported. It is implemented in FieldPathMatcher
 * class.
 *
 * The onMatched method is called when the specified XPath matches the current element.
 * Derived classes should implement this method to do something useful.
 *
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
abstract class PathMatcher(owner: IDConstraintChecker, paths: Array[XPath]) extends MatcherBundle(owner):

  /**
   * a flag that indicates that this element/attribute matches the path expression.
   * This flag is set by one of the child SinglePathMatcher.
   */
  private var matchFound: Boolean = false

  children = new Array[Matcher](paths.length)
  var i = 0
  while i < paths.length do
    children(i) = new SinglePathMatcher(paths(i))
    i += 1

  /**
   * this method should be called immediately after the installment of this PathMatcher.
   */
  @throws(classOf[SAXException])
  protected[identity] def start(namespaceURI: String, localName: String): Unit =
    if matchFound then
      onElementMatched(namespaceURI, localName)
    matchFound = false

  /**
   * this method is called when the element matches the XPath.
   */
  @throws(classOf[SAXException])
  protected[identity] def onElementMatched(namespaceURI: String, localName: String): Unit

  /**
   * this method is called when the attribute matches the XPath.
   */
  @throws(classOf[SAXException])
  protected[identity] def onAttributeMatched(
    namespaceURI: String,
    localName: String,
    value: String,
    `type`: Datatype
  ): Unit

  @throws(classOf[SAXException])
  override protected[identity] def startElement(namespaceURI: String, localName: String): Unit =
    super.startElement(namespaceURI, localName)
    if matchFound then
      onElementMatched(namespaceURI, localName)
    matchFound = false

  @throws(classOf[SAXException])
  override protected[identity] def onAttribute(
    namespaceURI: String,
    localName: String,
    value: String,
    `type`: Datatype
  ): Unit =
    super.onAttribute(namespaceURI, localName, value, `type`)
    if matchFound then
      onAttributeMatched(namespaceURI, localName, value, `type`)
    matchFound = false

  /**
   * the XPath matching engine for single Path.
   */
  private class SinglePathMatcher(protected val path: XPath) extends Matcher(PathMatcher.this.owner):

    /**
     * stores matched steps.
     * first dimension is expanded as the depth goes deep.
     * second dimension is always equal to the size of steps.
     */
    private var activeSteps: Array[Array[Boolean]] = new Array[Array[Boolean]](4)

    /**
     * this flag is set to true when the path contains an attribute step
     * and the current element matches the element part of the path.
     */
    private var elementMatched: Boolean = false

    activeSteps(0) = new Array[Boolean](path.steps.length + 1)
    activeSteps(0)(0) = true

    if path.steps.length == 0 then
      if path.attributeStep == null then
        matchFound = true
      else
        elementMatched = true

    @throws(classOf[SAXException])
    override protected[identity] def startElement(namespaceURI: String, localName: String): Unit =
      elementMatched = false

      val depth = getDepth()

      if depth == activeSteps.length - 1 then
        val newBuf = new Array[Array[Boolean]](depth * 2)
        System.arraycopy(activeSteps, 0, newBuf, 0, activeSteps.length)
        activeSteps = newBuf

      val len = path.steps.length
      val prvBuf = activeSteps(depth - 1)
      var curBuf = activeSteps(depth)
      if curBuf == null then
        curBuf = new Array[Boolean](len + 1)
        activeSteps(depth) = curBuf

      // shift to right
      if len != 0 then
        System.arraycopy(prvBuf, 0, curBuf, 1, len)
        curBuf(0) = path.isAnyDescendant

      // perform name test and deactivate unmatched steps
      var idx = 1
      while idx <= len do
        if curBuf(idx) && !path.steps(idx - 1).accepts(namespaceURI, localName) then
          curBuf(idx) = false
        idx += 1

      if curBuf(len) then
        if path.attributeStep == null then
          matchFound = true
        else
          elementMatched = true

    @throws(classOf[SAXException])
    override protected[identity] def onAttribute(
      namespaceURI: String,
      localName: String,
      value: String,
      `type`: Datatype
    ): Unit =
      if !elementMatched then return

      if path.attributeStep.accepts(namespaceURI, localName) then
        matchFound = true

    override protected[identity] def endElement(dt: Datatype): Unit =
      elementMatched = false
