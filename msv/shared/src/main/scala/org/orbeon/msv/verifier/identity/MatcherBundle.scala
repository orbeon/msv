package org.orbeon.msv.verifier.identity

import org.orbeon.msv.relaxng.datatype.Datatype
import org.xml.sax.SAXException
import scala.compiletime.uninitialized

/**
 * Base implementation of Matcher coordinator.
 *
 * This class behaves as a parent of several other matchers, or as a composite
 * XPath matcher.
 * Those child matchers are not directly registered to IDConstraintChecker.
 * Instead, they receive notifications through this object.
 *
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class MatcherBundle(owner: IDConstraintChecker) extends Matcher(owner):

  /** child matchers. */
  var children: Array[Matcher] = uninitialized

  /** depth. */
  private var depth: Int = 0
  protected final def getDepth(): Int = depth

  @throws(classOf[SAXException])
  override protected[identity] def startElement(namespaceURI: String, localName: String): Unit =
    depth += 1
    var i = 0
    while i < children.length do
      children(i).startElement(namespaceURI, localName)
      i += 1

  @throws(classOf[SAXException])
  override protected[identity] def onAttribute(
    namespaceURI: String,
    localName: String,
    value: String,
    `type`: Datatype
  ): Unit =
    var i = 0
    while i < children.length do
      children(i).onAttribute(namespaceURI, localName, value, `type`)
      i += 1

  @throws(classOf[SAXException])
  override protected[identity] def endElement(`type`: Datatype): Unit =
    var i = 0
    while i < children.length do
      children(i).endElement(`type`)
      i += 1
    depth -= 1
    if depth == -1 then
      // traversal complete.
      owner.remove(this)
      onRemoved()

  @throws(classOf[SAXException])
  override protected[identity] def characters(buf: Array[Char], start: Int, len: Int): Unit =
    var i = 0
    while i < children.length do
      children(i).characters(buf, start, len)
      i += 1

  /**
   * called when this bundle is deactivated.
   * This method is called by the endElement method when this bundle is
   * removed. A derived class can override this method to do whatever
   * necessary.
   */
  @throws(classOf[SAXException])
  protected[identity] def onRemoved(): Unit = ()
