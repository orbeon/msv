package org.orbeon.msv.grammar

/**
 * ExpressionVisitor that returns boolean.
 *
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
trait ExpressionVisitorBoolean {
  def onAttribute(exp: AttributeExp): Boolean
  def onChoice(exp: ChoiceExp): Boolean
  def onElement(exp: ElementExp): Boolean
  def onOneOrMore(exp: OneOrMoreExp): Boolean
  def onMixed(exp: MixedExp): Boolean
  def onList(exp: ListExp): Boolean
  def onRef(exp: ReferenceExp): Boolean
  def onOther(exp: OtherExp): Boolean
  def onEpsilon(): Boolean
  def onNullSet(): Boolean
  def onAnyString(): Boolean
  def onSequence(exp: SequenceExp): Boolean
  def onData(exp: DataExp): Boolean
  def onValue(exp: ValueExp): Boolean
  def onConcur(p: ConcurExp): Boolean
  def onInterleave(p: InterleaveExp): Boolean
}
