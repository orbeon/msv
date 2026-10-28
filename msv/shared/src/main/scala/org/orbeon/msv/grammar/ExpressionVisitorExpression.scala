package org.orbeon.msv.grammar

/**
 * ExpressionVisitor that returns Expression.
 *
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
trait ExpressionVisitorExpression {
  def onAttribute(exp: AttributeExp): Expression
  def onChoice(exp: ChoiceExp): Expression
  def onElement(exp: ElementExp): Expression
  def onOneOrMore(exp: OneOrMoreExp): Expression
  def onMixed(exp: MixedExp): Expression
  def onList(exp: ListExp): Expression
  def onRef(exp: ReferenceExp): Expression
  def onOther(exp: OtherExp): Expression
  def onEpsilon(): Expression
  def onNullSet(): Expression
  def onAnyString(): Expression
  def onSequence(exp: SequenceExp): Expression
  def onData(exp: DataExp): Expression
  def onValue(exp: ValueExp): Expression
  def onConcur(p: ConcurExp): Expression
  def onInterleave(p: InterleaveExp): Expression
}
