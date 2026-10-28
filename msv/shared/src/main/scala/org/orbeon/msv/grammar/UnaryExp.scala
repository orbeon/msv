package org.orbeon.msv.grammar

/**
 * Base implementation for those expression who has one child expression.
 *
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
abstract class UnaryExp(val exp: Expression) extends Expression(exp.hashCode) {
  protected final def calcHashCode(): Int = exp.hashCode

  override def equals(o: Any): Boolean = {
    if (this.getClass != o.getClass) return false
    o.asInstanceOf[UnaryExp].exp eq exp
  }
}
