package org.orbeon.msv.verifier.regexp

import org.orbeon.msv.grammar.{AttributeExp, ElementExp, Expression, ExpressionCloner, ExpressionPool, OtherExp, ReferenceExp}

/**
 * Creates an expression whose AttributeExp is completely replaced by nullSet.
 *
 * This step is used to remove all unconsumed AttributeExp from the expression.
 *
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class AttributePruner(pool: ExpressionPool) extends ExpressionCloner(pool):

  override def onAttribute(exp: AttributeExp): Expression = Expression.nullSet
  override def onRef(exp: ReferenceExp): Expression = exp.exp.visit(this)
  override def onOther(exp: OtherExp): Expression = exp.exp.visit(this)
  override def onElement(exp: ElementExp): Expression = exp

  final def prune(exp: Expression): Expression =
    // check the cache first.
    var ot = exp.verifierTag.asInstanceOf[OptimizationTag]
    if ot == null then
      ot = new OptimizationTag()
      exp.verifierTag = ot
    else if ot.attributePrunedExpression != null then
      return ot.attributePrunedExpression

    // cache miss. compute it.
    val r = exp.visit(this)
    ot.attributePrunedExpression = r // cache this result
    r
