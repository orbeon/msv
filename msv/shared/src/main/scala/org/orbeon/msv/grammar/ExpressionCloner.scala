package org.orbeon.msv.grammar

/**
 * clones an expression.
 *
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
abstract class ExpressionCloner(protected val pool: ExpressionPool) extends ExpressionVisitorExpression {
  def onChoice(exp: ChoiceExp): Expression = {
    val np1 = exp.exp1.visit(this)
    val np2 = exp.exp2.visit(this)
    if ((exp.exp1 eq np1) && (exp.exp2 eq np2)) exp
    else pool.createChoice(np1, np2)
  }

  def onOneOrMore(exp: OneOrMoreExp): Expression = {
    val np = exp.exp.visit(this)
    if (exp.exp eq np) exp
    else pool.createOneOrMore(np)
  }

  def onMixed(exp: MixedExp): Expression = {
    val body = exp.exp.visit(this)
    if (exp.exp eq body) exp
    else pool.createMixed(body)
  }

  def onList(exp: ListExp): Expression = {
    val body = exp.exp.visit(this)
    if (exp.exp eq body) exp
    else pool.createList(body)
  }

  def onSequence(exp: SequenceExp): Expression = {
    val np1 = exp.exp1.visit(this)
    val np2 = exp.exp2.visit(this)
    if ((exp.exp1 eq np1) && (exp.exp2 eq np2)) exp
    else pool.createSequence(np1, np2)
  }

  def onConcur(exp: ConcurExp): Expression =
    pool.createConcur(exp.exp1.visit(this), exp.exp2.visit(this))

  def onInterleave(exp: InterleaveExp): Expression =
    pool.createInterleave(exp.exp1.visit(this), exp.exp2.visit(this))

  def onEpsilon(): Expression = Expression.epsilon
  def onNullSet(): Expression = Expression.nullSet
  def onAnyString(): Expression = Expression.anyString
  def onData(exp: DataExp): Expression = exp
  def onValue(exp: ValueExp): Expression = exp
}
