package org.orbeon.msv.verifier.regexp

import org.orbeon.msv.grammar.{AttributeExp, ChoiceExp, ConcurExp, DataExp, ElementExp, Expression, ExpressionPool, ExpressionVisitorExpression, InterleaveExp, ListExp, MixedExp, OneOrMoreExp, OtherExp, ReferenceExp, SequenceExp, ValueExp}
import scala.compiletime.uninitialized

/**
 * computes regular expression derivative.
 *
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class ResidualCalculator(protected val pool: ExpressionPool) extends ExpressionVisitorExpression {
  protected var token: Token = uninitialized

  final def calcResidual(exp: Expression, token: ElementToken): Expression = {
    if (token.acceptedPatterns != null && token.acceptedPatterns.length == 1) {
      var ot: OptimizationTag = null
      if (exp.verifierTag == null) {
        ot = new OptimizationTag()
        exp.verifierTag = ot
      } else {
        ot = exp.verifierTag.asInstanceOf[OptimizationTag]
        val residual = ot.simpleElementTokenResidual.get(token.acceptedPatterns(0)).asInstanceOf[Expression]
        if (residual != null)
          return residual
      }

      this.token = token
      val residual = exp.visit(this)
      ot.simpleElementTokenResidual.put(token.acceptedPatterns(0), residual)
      return residual
    }

    this.token = token
    exp.visit(this)
  }

  final def calcResidual(exp: Expression, token: Token): Expression = {
    if (token.isInstanceOf[ElementToken])
      return calcResidual(exp, token.asInstanceOf[ElementToken])

    val oldToken = this.token
    this.token = token
    try {
      var r = exp.visit(this)

      if (token.isIgnorable)
        r = pool.createChoice(r, exp)

      r
    } finally {
      this.token = oldToken
    }
  }

  def onAttribute(exp: AttributeExp): Expression = {
    if (token.`match`(exp)) Expression.epsilon
    else Expression.nullSet
  }

  def onChoice(exp: ChoiceExp): Expression =
    pool.createChoice(exp.exp1.visit(this), exp.exp2.visit(this))

  def onElement(exp: ElementExp): Expression = {
    if (token.`match`(exp)) Expression.epsilon
    else Expression.nullSet
  }

  def onOneOrMore(exp: OneOrMoreExp): Expression =
    pool.createSequence(exp.exp.visit(this), pool.createZeroOrMore(exp.exp))

  def onMixed(exp: MixedExp): Expression = {
    if (token.matchAnyString()) exp
    else pool.createMixed(exp.exp.visit(this))
  }

  def onEpsilon(): Expression = Expression.nullSet
  def onNullSet(): Expression = Expression.nullSet

  def onAnyString(): Expression = {
    if (token.matchAnyString()) Expression.anyString
    else Expression.nullSet
  }

  def onRef(exp: ReferenceExp): Expression =
    exp.exp.visit(this)

  def onOther(exp: OtherExp): Expression =
    exp.exp.visit(this)

  def onSequence(exp: SequenceExp): Expression = {
    val r = pool.createSequence(exp.exp1.visit(this), exp.exp2)
    if (exp.exp1.isEpsilonReducible)
      pool.createChoice(r, exp.exp2.visit(this))
    else
      r
  }

  def onData(exp: DataExp): Expression = {
    if (token.`match`(exp)) Expression.epsilon
    else Expression.nullSet
  }

  def onValue(exp: ValueExp): Expression = {
    if (token.`match`(exp)) Expression.epsilon
    else Expression.nullSet
  }

  def onList(exp: ListExp): Expression = {
    if (token.`match`(exp)) Expression.epsilon
    else Expression.nullSet
  }

  def onConcur(exp: ConcurExp): Expression =
    pool.createConcur(exp.exp1.visit(this), exp.exp2.visit(this))

  def onInterleave(exp: InterleaveExp): Expression = {
    pool.createChoice(
      pool.createInterleave(exp.exp1.visit(this), exp.exp2),
      pool.createInterleave(exp.exp1, exp.exp2.visit(this))
    )
  }
}
