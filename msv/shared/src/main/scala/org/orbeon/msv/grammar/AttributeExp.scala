package org.orbeon.msv.grammar

/**
 * Attribute declaration.
 *
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class AttributeExp(val nameClass: NameClass, val exp: Expression, val defaultValue: String = null)
  extends Expression(nameClass.hashCode + exp.hashCode + (if (defaultValue != null) defaultValue.hashCode else 0)) with NameClassAndExpression {

  def getNameClass: NameClass = nameClass
  def getContentModel: Expression = exp
  def getDefaultValue: String = defaultValue

  protected final def calcHashCode(): Int = {
    var hash = nameClass.hashCode + exp.hashCode
    if (defaultValue != null) hash += defaultValue.hashCode
    hash
  }

  override def equals(o: Any): Boolean = {
    if (o == null || o.getClass != classOf[AttributeExp]) return false
    val rhs = o.asInstanceOf[AttributeExp]
    if (rhs.nameClass.equals(nameClass) && (rhs.exp eq exp)) {
      if (defaultValue != null) defaultValue == rhs.defaultValue
      else rhs.defaultValue == null
    } else {
      false
    }
  }

  def visit(visitor: ExpressionVisitor): AnyRef = visitor.onAttribute(this)
  def visit(visitor: ExpressionVisitorExpression): Expression = visitor.onAttribute(this)
  def visit(visitor: ExpressionVisitorBoolean): Boolean = visitor.onAttribute(this)
  def visit(visitor: ExpressionVisitorVoid): Unit = visitor.onAttribute(this)

  protected def calcEpsilonReducibility(): Boolean = false
}
