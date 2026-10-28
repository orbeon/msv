package org.orbeon.msv.grammar

/**
 * &lt;concur&gt; pattern of TREX.
 *
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
final class ConcurExp(left: Expression, right: Expression) extends BinaryExp(left, right) {
  def visit(visitor: ExpressionVisitor): AnyRef = visitor.onConcur(this)
  def visit(visitor: ExpressionVisitorExpression): Expression = visitor.onConcur(this)
  def visit(visitor: ExpressionVisitorBoolean): Boolean = visitor.onConcur(this)
  def visit(visitor: ExpressionVisitorVoid): Unit = visitor.onConcur(this)

  protected def calcEpsilonReducibility(): Boolean =
    exp1.isEpsilonReducible && exp2.isEpsilonReducible
}
