package org.orbeon.msv.verifier.identity

import org.orbeon.msv.driver.textui.Debug
import org.orbeon.msv.grammar.xmlschema.IdentityConstraint
import org.orbeon.msv.relaxng.datatype.Datatype
import org.xml.sax.SAXException

/**
 * XPath matcher that tests the selector of an identity constraint.
 *
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class SelectorMatcher(
  owner: IDConstraintChecker,
  val idConst: IdentityConstraint,
  namespaceURI: String,
  localName: String
) extends PathMatcher(owner, idConst.selectors):

  // register this scope as active.
  owner.pushActiveScope(idConst, this)

  if Debug.debug then
    println(s"new id scope is available for {${idConst.localName}}")

  start(namespaceURI, localName)

  @throws(classOf[SAXException])
  override protected[identity] def onRemoved(): Unit =
    super.onRemoved()
    // this scope is no longer active.
    owner.popActiveScope(idConst, this)

  @throws(classOf[SAXException])
  override protected[identity] def onElementMatched(namespaceURI: String, localName: String): Unit =
    if Debug.debug then
      println("find a match for a selector: " + idConst.localName)

    // this element matches the path.
    owner.add(new FieldsMatcher(this, namespaceURI, localName))

  override protected[identity] def onAttributeMatched(
    namespaceURI: String,
    localName: String,
    value: String,
    `type`: Datatype
  ): Unit =
    // selectors cannot contain attribute steps.
    throw new Error()
