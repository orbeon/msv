package org.orbeon.msv.reader

import org.orbeon.msv.grammar.Expression
import org.orbeon.msv.util.StartTagInfo

/**
 * State that parses Expression which contains other expressions.
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
abstract class ExpressionWithChildState extends ExpressionState with ExpressionOwner:

  protected var exp: Expression = scala.compiletime.uninitialized

  override protected def startSelf(): Unit =
    super.startSelf()
    exp = initialExpression()

  protected def initialExpression(): Expression = null

  protected def defaultExpression(): Expression = null

  final def onEndChild(childExpression: Expression): Unit =
    exp = castExpression(exp, childExpression)

  protected final def makeExpression(): Expression =
    if exp == null then
      exp = defaultExpression()

    if exp == null then
      reader.reportError(GrammarReader.ERR_MISSING_CHILD_EXPRESSION)
      exp = Expression.nullSet

    annealExpression(exp)

  protected def createChildState(tag: StartTagInfo): State =
    reader.createExpressionChildState(this, tag)

  protected def castExpression(halfCastedExpression: Expression, newChildExpression: Expression): Expression

  protected def annealExpression(exp: Expression): Expression = exp
