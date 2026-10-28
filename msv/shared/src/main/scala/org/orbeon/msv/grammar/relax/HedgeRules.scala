package org.orbeon.msv.grammar.relax

import org.orbeon.msv.grammar.{Expression, ExpressionPool, ReferenceExp}

/**
 * hedgeRule of RELAX module.
 * 
 * ReferenceExp.exp holds a choice of the content models of all hedgeRules that
 * share the same label name.
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class HedgeRules protected[relax] (
  label: String,
  val ownerModule: RELAXModule
) extends ReferenceExp(label) with Exportable:

  def addHedge(newExp: Expression, pool: ExpressionPool): Unit =
    if this.exp == null then this.exp = newExp
    else this.exp = pool.createChoice(this.exp, newExp)

  override def equals(o: Any): Boolean = this.asInstanceOf[AnyRef] eq o.asInstanceOf[AnyRef]

  def visit(visitor: RELAXExpressionVisitor): AnyRef =
    visitor.onHedgeRules(this)

  def visit(visitor: RELAXExpressionVisitorExpression): Expression =
    visitor.onHedgeRules(this)

  def visit(visitor: RELAXExpressionVisitorBoolean): Boolean =
    visitor.onHedgeRules(this)

  def visit(visitor: RELAXExpressionVisitorVoid): Unit =
    visitor.onHedgeRules(this)

  /**
   * a flag that indicates this hedgeRule is exported and
   * therefore accessible from other modules.
   */
  var exported: Boolean = false
  def isExported(): Boolean = exported
