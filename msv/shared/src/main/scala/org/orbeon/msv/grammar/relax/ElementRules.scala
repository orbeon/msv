package org.orbeon.msv.grammar.relax

import org.orbeon.msv.grammar.{Expression, ExpressionPool, ReferenceExp}

/**
 * Set of ElementRule objects that share the label name.
 * 
 * ReferenceExp.exp contains choice of ElementRule objects.
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class ElementRules protected[relax] (
  label: String,
  val ownerModule: RELAXModule
) extends ReferenceExp(label) with Exportable:

  override def equals(o: Any): Boolean = this.asInstanceOf[AnyRef] eq o.asInstanceOf[AnyRef]

  override protected def calcEpsilonReducibility(): Boolean =
    // elementRules are always not epsilon-reducible.
    false

  def addElementRule(pool: ExpressionPool, newRule: ElementRule): Unit =
    newRule.parent = this
    if exp == null then
      exp = newRule
    else
      exp = pool.createChoice(exp, newRule)

  def visit(visitor: RELAXExpressionVisitor): AnyRef =
    visitor.onElementRules(this)

  def visit(visitor: RELAXExpressionVisitorExpression): Expression =
    visitor.onElementRules(this)

  def visit(visitor: RELAXExpressionVisitorBoolean): Boolean =
    visitor.onElementRules(this)

  def visit(visitor: RELAXExpressionVisitorVoid): Unit =
    visitor.onElementRules(this)

  /**
   * a flag that indicates this elementRule is exported and
   * therefore accessible from other modules.
   */
  var exported: Boolean = false
  def isExported(): Boolean = exported
