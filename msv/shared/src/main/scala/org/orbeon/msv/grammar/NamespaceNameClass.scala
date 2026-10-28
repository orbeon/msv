package org.orbeon.msv.grammar

/**
 * a NameClass that accepts any name as long as its namespace URI matches to the specified one.
 *
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
final class NamespaceNameClass(val namespaceURI: String) extends NameClass {
  def accepts(namespaceURI: String, localName: String): Boolean =
    this.namespaceURI == namespaceURI || namespaceURI == NameClass.NAMESPACE_WILDCARD

  def visit(visitor: NameClassVisitor): AnyRef = visitor.onNsName(this)

  override def toString: String = "{" + namespaceURI + "}*"
}
