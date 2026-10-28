package org.orbeon.msv.grammar

/**
 * &lt;interleave&gt; pattern of TREX, or &lt;all&gt; particle of XML Schema.
 *
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
final class InterleaveExp(left: Expression, right: Expression) extends BinaryExp(left, right) {
  def visit(visitor: ExpressionVisitor): AnyRef = visitor.onInterleave(this)
  def visit(visitor: ExpressionVisitorExpression): Expression = visitor.onInterleave(this)
  def visit(visitor: ExpressionVisitorBoolean): Boolean = visitor.onInterleave(this)
  def visit(visitor: ExpressionVisitorVoid): Unit = visitor.onInterleave(this)

  protected def calcEpsilonReducibility(): Boolean =
    exp1.isEpsilonReducible && exp2.isEpsilonReducible
}
