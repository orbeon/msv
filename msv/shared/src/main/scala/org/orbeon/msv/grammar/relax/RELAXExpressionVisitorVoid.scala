package org.orbeon.msv.grammar.relax

import org.orbeon.msv.grammar.ExpressionVisitorVoid

/**
 * RELAX Version of [[ExpressionVisitorVoid]]
 *
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
trait RELAXExpressionVisitorVoid extends ExpressionVisitorVoid:
  // RELAX visitor can ignore onRef callback.
  def onAttPool(exp: AttPoolClause): Unit
  def onTag(exp: TagClause): Unit
  def onElementRules(exp: ElementRules): Unit
  def onHedgeRules(exp: HedgeRules): Unit
