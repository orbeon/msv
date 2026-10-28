package org.orbeon.msv.grammar

/**
 * NameClass that acts as a not operator.
 *
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
final class NotNameClass(val child: NameClass) extends NameClass {
  /**
   * accepts a name if it is not accepted by the child name class.
   */
  def accepts(namespaceURI: String, localName: String): Boolean =
    !child.accepts(namespaceURI, localName)

  def visit(visitor: NameClassVisitor): AnyRef = visitor.onNot(this)

  override def toString: String = "~" + child.toString
}
