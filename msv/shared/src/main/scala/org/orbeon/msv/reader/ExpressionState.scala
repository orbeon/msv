package org.orbeon.msv.reader

import org.orbeon.msv.grammar.Expression

/**
 * Base implementation for those states who read tags representing an expression.
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
abstract class ExpressionState extends SimpleState:

  override protected def endSelf(): Unit =
    val exp = reader.interceptExpression(this, makeExpression())
    if parentState != null then
      parentState.asInstanceOf[ExpressionOwner].onEndChild(exp)
    super.endSelf()

  protected def makeExpression(): Expression
