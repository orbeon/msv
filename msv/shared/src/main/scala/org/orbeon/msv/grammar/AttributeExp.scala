package org.orbeon.msv.grammar

/**
 * Attribute declaration.
 *
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class AttributeExp(val nameClass: NameClass, val exp: Expression)
  extends Expression(nameClass.hashCode + exp.hashCode) with NameClassAndExpression {

  def getNameClass: NameClass = nameClass
  def getContentModel: Expression = exp

  protected final def calcHashCode(): Int =
    nameClass.hashCode + exp.hashCode

  override def equals(o: Any): Boolean = {
    if (o.getClass != classOf[AttributeExp]) return false
    val rhs = o.asInstanceOf[AttributeExp]
    rhs.nameClass.equals(nameClass) && (rhs.exp eq exp)
  }

  def visit(visitor: ExpressionVisitor): AnyRef = visitor.onAttribute(this)
  def visit(visitor: ExpressionVisitorExpression): Expression = visitor.onAttribute(this)
  def visit(visitor: ExpressionVisitorBoolean): Boolean = visitor.onAttribute(this)
  def visit(visitor: ExpressionVisitorVoid): Unit = visitor.onAttribute(this)

  protected def calcEpsilonReducibility(): Boolean = false
}
