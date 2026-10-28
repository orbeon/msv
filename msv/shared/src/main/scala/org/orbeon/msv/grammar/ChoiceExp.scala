package org.orbeon.msv.grammar

/**
 * A|B.
 *
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
final class ChoiceExp(left: Expression, right: Expression) extends BinaryExp(left, right) {
  def visit(visitor: ExpressionVisitor): AnyRef = visitor.onChoice(this)
  def visit(visitor: ExpressionVisitorExpression): Expression = visitor.onChoice(this)
  def visit(visitor: ExpressionVisitorBoolean): Boolean = visitor.onChoice(this)
  def visit(visitor: ExpressionVisitorVoid): Unit = visitor.onChoice(this)

  protected def calcEpsilonReducibility(): Boolean =
    exp1.isEpsilonReducible || exp2.isEpsilonReducible
}
