package org.orbeon.msv.grammar

/**
 * &lt;list&gt; of RELAX NG.
 *
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
final class ListExp(exp: Expression) extends UnaryExp(exp) {
  def visit(visitor: ExpressionVisitor): AnyRef = visitor.onList(this)
  def visit(visitor: ExpressionVisitorExpression): Expression = visitor.onList(this)
  def visit(visitor: ExpressionVisitorBoolean): Boolean = visitor.onList(this)
  def visit(visitor: ExpressionVisitorVoid): Unit = visitor.onList(this)

  protected def calcEpsilonReducibility(): Boolean =
    exp.isEpsilonReducible
}
