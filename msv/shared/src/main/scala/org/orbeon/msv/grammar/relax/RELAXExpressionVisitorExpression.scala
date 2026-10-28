package org.orbeon.msv.grammar.relax

import org.orbeon.msv.grammar.{Expression, ExpressionVisitorExpression}

/**
 * RELAX version of [[ExpressionVisitorExpression]]
 *
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
trait RELAXExpressionVisitorExpression extends ExpressionVisitorExpression:
  // RELAX visitor can ignore onRef callback.
  def onAttPool(exp: AttPoolClause): Expression
  def onTag(exp: TagClause): Expression
  def onElementRules(exp: ElementRules): Expression
  def onHedgeRules(exp: HedgeRules): Expression
