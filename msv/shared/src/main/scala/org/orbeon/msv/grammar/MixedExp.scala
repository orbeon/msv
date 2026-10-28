package org.orbeon.msv.grammar

/**
 * &lt;mixed&gt; of RELAX.
 *
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
final class MixedExp(exp: Expression) extends UnaryExp(exp) {
  def visit(visitor: ExpressionVisitor): AnyRef = visitor.onMixed(this)
  def visit(visitor: ExpressionVisitorExpression): Expression = visitor.onMixed(this)
  def visit(visitor: ExpressionVisitorBoolean): Boolean = visitor.onMixed(this)
  def visit(visitor: ExpressionVisitorVoid): Unit = visitor.onMixed(this)

  protected def calcEpsilonReducibility(): Boolean =
    exp.isEpsilonReducible
}
