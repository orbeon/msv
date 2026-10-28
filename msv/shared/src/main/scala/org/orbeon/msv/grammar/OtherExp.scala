package org.orbeon.msv.grammar

/**
 * Base class for [[Expression]]s that are not defined in AGM.
 *
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class OtherExp(var exp: Expression) extends Expression {
  def this() = this(null)

  def printName(): String = {
    val className = this.getClass.getName
    val idx = className.lastIndexOf('.')
    if (idx >= 0) className.substring(idx + 1) else className
  }

  protected final def calcHashCode(): Int = System.identityHashCode(this)

  override def equals(o: Any): Boolean = this eq o.asInstanceOf[AnyRef]

  protected def calcEpsilonReducibility(): Boolean = {
    if (exp == null) false
    else exp.isEpsilonReducible
  }

  final def visit(visitor: ExpressionVisitor): AnyRef = visitor.onOther(this)
  final def visit(visitor: ExpressionVisitorExpression): Expression = visitor.onOther(this)
  final def visit(visitor: ExpressionVisitorBoolean): Boolean = visitor.onOther(this)
  final def visit(visitor: ExpressionVisitorVoid): Unit = visitor.onOther(this)
}
