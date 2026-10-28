package org.orbeon.msv.grammar

/**
 * Reference to the other expression.
 *
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class ReferenceExp(val name: String, var exp: Expression) extends Expression {
  def this(name: String) = this(name, null)

  override def isEpsilonReducible: Boolean = {
    if (exp == null) false
    else exp.isEpsilonReducible
  }

  protected final def calcHashCode(): Int = System.identityHashCode(this)

  def isDefined: Boolean = exp != null

  override def equals(o: Any): Boolean = this eq o.asInstanceOf[AnyRef]

  protected def calcEpsilonReducibility(): Boolean = {
    if (exp == null) false
    else exp.isEpsilonReducible
  }

  final def visit(visitor: ExpressionVisitor): AnyRef = visitor.onRef(this)
  final def visit(visitor: ExpressionVisitorExpression): Expression = visitor.onRef(this)
  final def visit(visitor: ExpressionVisitorBoolean): Boolean = visitor.onRef(this)
  final def visit(visitor: ExpressionVisitorVoid): Unit = visitor.onRef(this)
}
