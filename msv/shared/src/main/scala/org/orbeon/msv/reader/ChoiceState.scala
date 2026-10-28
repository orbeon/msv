package org.orbeon.msv.reader

import org.orbeon.msv.grammar.Expression

/**
 * state that creates ChoiceExp.
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class ChoiceState(protected var allowEmptyChoice: Boolean) extends ExpressionWithChildState:
  def this() = this(false)

  override protected def initialExpression(): Expression =
    if allowEmptyChoice then Expression.nullSet else null

  protected def castExpression(exp: Expression, child: Expression): Expression =
    if exp == null then child
    else reader.pool.createChoice(exp, child)
