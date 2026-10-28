package org.orbeon.msv.grammar.relax

import org.orbeon.msv.grammar.ExpressionVisitorBoolean

/**
 * RELAX version of [[ExpressionVisitorBoolean]].
 *
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
trait RELAXExpressionVisitorBoolean extends ExpressionVisitorBoolean:
  // RELAX visitor can ignore onRef callback.
  def onAttPool(exp: AttPoolClause): Boolean
  def onTag(exp: TagClause): Boolean
  def onElementRules(exp: ElementRules): Boolean
  def onHedgeRules(exp: HedgeRules): Boolean
