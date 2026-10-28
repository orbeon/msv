package org.orbeon.msv.verifier.identity

import org.orbeon.msv.driver.textui.Debug
import org.orbeon.msv.grammar.xmlschema.{KeyConstraint, KeyRefConstraint}
import org.xml.sax.Locator
import org.xml.sax.SAXException
import org.xml.sax.helpers.LocatorImpl

/**
 * Coordinator of FieldMatcher.
 *
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class FieldsMatcher(
  val selector: SelectorMatcher,
  namespaceURI: String,
  localName: String
) extends MatcherBundle(selector.owner):

  /**
   * location of the start tag.
   * It is usually preferable as a source of error.
   */
  protected val startTag: Locator =
    if owner.getLocator == null then null
    else new LocatorImpl(owner.getLocator)

  children = new Array[Matcher](selector.idConst.fields.length)
  var idx = 0
  while idx < selector.idConst.fields.length do
    children(idx) = new FieldMatcher(this, selector.idConst.fields(idx), namespaceURI, localName)
    idx += 1

  @throws(classOf[SAXException])
  override protected[identity] def onRemoved(): Unit =
    val values = new Array[AnyRef](children.length)

    // copy matched values into "values" variable,
    // while checking any unmatched fields.
    var i = 0
    while i < children.length do
      val v = children(i).asInstanceOf[FieldMatcher].value
      values(i) = v
      if v == null then
        if !selector.idConst.isInstanceOf[KeyConstraint] then
          // some fields didn't match to anything.
          // In case of KeyRef and Unique constraints,
          // we can ignore this node.
          return

        // if this is the key constraint, it is an error
        owner.reportError(
          startTag,
          null,
          IDConstraintChecker.ERR_UNMATCHED_KEY_FIELD,
          Array(
            selector.idConst.namespaceURI,
            selector.idConst.localName,
            Integer.valueOf(i + 1)
          )
        )
        return
      i += 1

    if Debug.debug then
      println("fields collected for " + selector.idConst.localName)

    val kv = new KeyValue(values, startTag)
    if owner.addKeyValue(selector, kv) then
      return

    // the same value already exists.
    if selector.idConst.isInstanceOf[KeyRefConstraint] then
      // multiple reference to the same key value.
      // not a problem.
      return

    // find a value that collides with kv
    val items = owner.getKeyValues(selector)
    var collIndex = 0
    while collIndex < items.length do
      if items(collIndex) == kv then
        // found
        val collLocator = items(collIndex).locator
        owner.reportError(
          startTag,
          null,
          IDConstraintChecker.ERR_NOT_UNIQUE,
          Array(selector.idConst.namespaceURI, selector.idConst.localName)
        )
        owner.reportError(
          collLocator,
          null,
          IDConstraintChecker.ERR_NOT_UNIQUE_DIAG,
          Array(selector.idConst.namespaceURI, selector.idConst.localName)
        )
        return
      collIndex += 1
