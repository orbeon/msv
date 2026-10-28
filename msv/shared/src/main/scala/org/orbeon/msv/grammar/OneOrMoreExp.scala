package org.orbeon.msv.grammar

/**
 * A+.
 *
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
final class OneOrMoreExp(exp: Expression) extends UnaryExp(exp) {
  def visit(visitor: ExpressionVisitor): AnyRef = visitor.onOneOrMore(this)
  def visit(visitor: ExpressionVisitorExpression): Expression = visitor.onOneOrMore(this)
  def visit(visitor: ExpressionVisitorBoolean): Boolean = visitor.onOneOrMore(this)
  def visit(visitor: ExpressionVisitorVoid): Unit = visitor.onOneOrMore(this)

  protected def calcEpsilonReducibility(): Boolean =
    exp.isEpsilonReducible
}
