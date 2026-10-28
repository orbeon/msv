package org.orbeon.msv.util

import org.orbeon.msv.grammar.SimpleNameClass

/**
 * pair of Strings.
 *
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
final class StringPair(val namespaceURI: String, val localName: String) extends Serializable {
  if (namespaceURI == null)
    throw new InternalError("namespace URI is null")
  if (localName == null)
    throw new InternalError("local name is null")

  def this(name: SimpleNameClass) = this(name.namespaceURI, name.localName)

  override def equals(o: Any): Boolean = o match {
    case other: StringPair =>
      namespaceURI == other.namespaceURI && localName == other.localName
    case _ => false
  }

  override def hashCode(): Int = namespaceURI.hashCode ^ localName.hashCode

  override def toString: String = "{" + namespaceURI + "}" + localName
}
