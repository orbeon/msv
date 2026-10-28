package org.orbeon.msv.grammar

/**
 * Visitor interface for [[Expression]] and its derived types.
 *
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
trait ExpressionVisitor {
  def onAttribute(exp: AttributeExp): AnyRef
  def onChoice(exp: ChoiceExp): AnyRef
  def onElement(exp: ElementExp): AnyRef
  def onOneOrMore(exp: OneOrMoreExp): AnyRef
  def onMixed(exp: MixedExp): AnyRef
  def onList(exp: ListExp): AnyRef
  def onRef(exp: ReferenceExp): AnyRef
  def onOther(exp: OtherExp): AnyRef
  def onEpsilon(): AnyRef
  def onNullSet(): AnyRef
  def onAnyString(): AnyRef
  def onSequence(exp: SequenceExp): AnyRef
  def onData(exp: DataExp): AnyRef
  def onValue(exp: ValueExp): AnyRef
  def onConcur(p: ConcurExp): AnyRef
  def onInterleave(p: InterleaveExp): AnyRef
}
