package org.orbeon.msv.grammar

import org.orbeon.msv.util.StringPair

/**
 * a NameClass that accepts only one fixed name.
 *
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
final class SimpleNameClass(val namespaceURI: String, val localName: String, val prefix: String = "") extends NameClass {
  def this(name: StringPair) = this(name.namespaceURI, name.localName, "")

  def accepts(namespaceURI: String, localName: String): Boolean = {
    (this.namespaceURI == namespaceURI || namespaceURI == NameClass.NAMESPACE_WILDCARD) &&
    (this.localName == localName || localName == NameClass.LOCALNAME_WILDCARD)
  }

  def visit(visitor: NameClassVisitor): AnyRef = visitor.onSimple(this)

  def toStringPair(): StringPair = new StringPair(namespaceURI, localName)

  override def toString: String =
    if prefix != null && prefix.nonEmpty then prefix + ":" + localName
    else "{" + namespaceURI + "}" + localName
}
