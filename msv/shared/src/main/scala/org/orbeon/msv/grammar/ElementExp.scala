package org.orbeon.msv.grammar

/**
 * Element declaration.
 *
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
abstract class ElementExp(
  var contentModel: Expression,
  var ignoreUndeclaredAttributes: Boolean
) extends Expression(if (contentModel != null) contentModel.hashCode else 0) with NameClassAndExpression {

  def getContentModel: Expression = contentModel

  def getNameClass: NameClass

  protected final def calcHashCode(): Int =
    if (contentModel != null) contentModel.hashCode else 0

  override def equals(o: Any): Boolean = this eq o.asInstanceOf[AnyRef]

  final def visit(visitor: ExpressionVisitor): AnyRef = visitor.onElement(this)
  final def visit(visitor: ExpressionVisitorExpression): Expression = visitor.onElement(this)
  final def visit(visitor: ExpressionVisitorBoolean): Boolean = visitor.onElement(this)
  final def visit(visitor: ExpressionVisitorVoid): Unit = visitor.onElement(this)

  protected final def calcEpsilonReducibility(): Boolean = false
}
