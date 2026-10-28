package org.orbeon.msv.verifier.identity

import org.xml.sax.Locator
import org.xml.sax.helpers.LocatorImpl

/**
 * represents multi-field keys.
 *
 * this class implements equality test and hash code based on
 * the equalities of each item.
 *
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class KeyValue(val values: Array[AnyRef], loc: Locator):

  /** source location that this value is found. */
  val locator: Locator =
    if loc == null then null
    else new LocatorImpl(loc)

  override def hashCode(): Int =
    var code = 0
    var i = 0
    while i < values.length do
      if values(i) != null then
        code ^= values(i).hashCode()
      i += 1
    code

  override def equals(o: Any): Boolean =
    if !o.isInstanceOf[KeyValue] then return false
    val rhs = o.asInstanceOf[KeyValue]
    if values.length != rhs.values.length then return false

    var i = 0
    while i < values.length do
      if values(i) != rhs.values(i) then return false
      i += 1
    true
