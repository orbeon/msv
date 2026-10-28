package org.orbeon.msv.grammar.dtd

import org.orbeon.msv.grammar.{NameClass, NameClassVisitor, SimpleNameClass}

/**
 * a NameClass that accepts any tag name as long as its local part is specified name.
 *
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
final class LocalNameClass(val localName: String) extends NameClass {
  def accepts(namespaceURI: String, localName: String): Boolean =
    this.localName == localName || NameClass.LOCALNAME_WILDCARD == localName

  def visit(visitor: NameClassVisitor): AnyRef =
    new SimpleNameClass("", localName).visit(visitor)

  override def toString: String = localName
}
