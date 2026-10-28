package org.orbeon.msv.verifier.regexp

import org.orbeon.msv.grammar.*

/**
 * removes all unnecessary expressions and
 * creates an expression that consists of required attributes and choices only.
 *
 * <XMP>
 * For example,
 *
 * <choice>
 *   <element />
 *   <attribute />
 * </choice>
 *
 * will be converted to
 *
 * <empty />
 *
 * because no attribute is required. But
 *
 * <choice>
 *   <attribute />
 *   <attribute />
 * </choice>
 *
 * will remain the same because one or the other is required.
 *
 * this method also removes SequenceExp.
 *
 * <sequence>
 *   <attribute name="A" />
 *   <attribute name="B" />
 * </sequence>
 *
 * will be converted to
 *
 * <attribute name="A" />
 *
 * This function object is used only for error recovery.
 * Resulting expressions always consist only of <choice>s and <attribute>s.
 * </XMP>
 *
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class AttributePicker(private val pool: ExpressionPool) extends ExpressionVisitorExpression:

  def onElement(exp: ElementExp): Expression = Expression.epsilon
  def onMixed(exp: MixedExp): Expression = exp.exp.visit(this)
  def onAnyString(): Expression = Expression.epsilon
  def onEpsilon(): Expression = Expression.epsilon
  def onNullSet(): Expression = Expression.nullSet
  def onRef(exp: ReferenceExp): Expression = exp.exp.visit(this)
  def onOther(exp: OtherExp): Expression = exp.exp.visit(this)
  def onData(exp: DataExp): Expression = Expression.epsilon
  def onValue(exp: ValueExp): Expression = Expression.epsilon
  def onList(exp: ListExp): Expression = Expression.epsilon
  def onAttribute(exp: AttributeExp): Expression = exp
  def onOneOrMore(exp: OneOrMoreExp): Expression = exp.exp.visit(this)

  def onSequence(exp: SequenceExp): Expression =
    val ex1 = exp.exp1.visit(this)
    val ex2 = exp.exp2.visit(this)
    if ex1.isEpsilonReducible then
      if ex2.isEpsilonReducible then Expression.epsilon
      else ex2
    else
      ex1

  def onInterleave(exp: InterleaveExp): Expression =
    val ex1 = exp.exp1.visit(this)
    val ex2 = exp.exp2.visit(this)
    if ex1.isEpsilonReducible then
      if ex2.isEpsilonReducible then Expression.epsilon
      else ex2
    else
      ex1

  def onConcur(exp: ConcurExp): Expression = Expression.epsilon

  def onChoice(exp: ChoiceExp): Expression =
    val ex1 = exp.exp1.visit(this)
    val ex2 = exp.exp2.visit(this)
    if ex1.isEpsilonReducible || ex2.isEpsilonReducible then
      Expression.epsilon
    else
      pool.createChoice(ex1, ex2)
