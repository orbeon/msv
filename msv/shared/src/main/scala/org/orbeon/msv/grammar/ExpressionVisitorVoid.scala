package org.orbeon.msv.grammar

/**
 * ExpressionVisitor that returns void.
 *
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
trait ExpressionVisitorVoid {
  def onAttribute(exp: AttributeExp): Unit
  def onChoice(exp: ChoiceExp): Unit
  def onElement(exp: ElementExp): Unit
  def onOneOrMore(exp: OneOrMoreExp): Unit
  def onMixed(exp: MixedExp): Unit
  def onList(exp: ListExp): Unit
  def onRef(exp: ReferenceExp): Unit
  def onOther(exp: OtherExp): Unit
  def onEpsilon(): Unit
  def onNullSet(): Unit
  def onAnyString(): Unit
  def onSequence(exp: SequenceExp): Unit
  def onData(exp: DataExp): Unit
  def onValue(exp: ValueExp): Unit
  def onConcur(p: ConcurExp): Unit
  def onInterleave(p: InterleaveExp): Unit
}
