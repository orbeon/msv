package org.orbeon.msv.grammar.relax

import org.orbeon.msv.grammar.ExpressionVisitor

/**
 * Visitor interface for RELAX expressions.
 *
 * By implementing this interface, your visitor can distinguish
 * four subclass of ReferenceExp introduced as RELAX stub.
 *
 * Note that onRef method may still be called if you visit AGM created from
 * TREX pattern.
 *
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
trait RELAXExpressionVisitor extends ExpressionVisitor:
  def onAttPool(exp: AttPoolClause): AnyRef
  def onTag(exp: TagClause): AnyRef
  def onElementRules(exp: ElementRules): AnyRef
  def onHedgeRules(exp: HedgeRules): AnyRef
