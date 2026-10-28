package org.orbeon.msv.grammar

/**
 * &lt;difference&gt; name class.
 *
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class DifferenceNameClass(val nc1: NameClass, val nc2: NameClass) extends NameClass {
  /**
   * accepts a name if it is accepted by nc1 and not by nc2.
   */
  def accepts(namespaceURI: String, localPart: String): Boolean =
    nc1.accepts(namespaceURI, localPart) && !nc2.accepts(namespaceURI, localPart)

  def visit(visitor: NameClassVisitor): AnyRef =
    visitor.onDifference(this)

  override def toString: String = nc1.toString + "-" + nc2.toString
}
