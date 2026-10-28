package org.orbeon.msv.grammar

/**
 * A,B. concatenation operator of the regular expression.
 *
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
final class SequenceExp(left: Expression, right: Expression) extends BinaryExp(left, right) {
  def visit(visitor: ExpressionVisitor): AnyRef = visitor.onSequence(this)
  def visit(visitor: ExpressionVisitorExpression): Expression = visitor.onSequence(this)
  def visit(visitor: ExpressionVisitorBoolean): Boolean = visitor.onSequence(this)
  def visit(visitor: ExpressionVisitorVoid): Unit = visitor.onSequence(this)

  protected def calcEpsilonReducibility(): Boolean =
    exp1.isEpsilonReducible && exp2.isEpsilonReducible
}
