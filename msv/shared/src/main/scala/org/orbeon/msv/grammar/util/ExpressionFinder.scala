package org.orbeon.msv.grammar.util

import org.orbeon.msv.grammar.{AttributeExp, ChoiceExp, ConcurExp, DataExp, ElementExp, ExpressionVisitorBoolean, InterleaveExp, ListExp, MixedExp, OneOrMoreExp, OtherExp, ReferenceExp, SequenceExp, ValueExp}

/**
 * Base class for "finding" something from an expression.
 *
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
abstract class ExpressionFinder extends ExpressionVisitorBoolean {
  def onSequence(exp: SequenceExp): Boolean = exp.exp1.visit(this) || exp.exp2.visit(this)
  def onInterleave(exp: InterleaveExp): Boolean = exp.exp1.visit(this) || exp.exp2.visit(this)
  def onConcur(exp: ConcurExp): Boolean = exp.exp1.visit(this) || exp.exp2.visit(this)
  def onChoice(exp: ChoiceExp): Boolean = exp.exp1.visit(this) || exp.exp2.visit(this)
  def onAttribute(exp: AttributeExp): Boolean = exp.exp.visit(this)
  def onElement(exp: ElementExp): Boolean = exp.contentModel.visit(this)
  def onOneOrMore(exp: OneOrMoreExp): Boolean = exp.exp.visit(this)
  def onMixed(exp: MixedExp): Boolean = exp.exp.visit(this)
  def onList(exp: ListExp): Boolean = exp.exp.visit(this)
  def onRef(exp: ReferenceExp): Boolean = exp.exp.visit(this)
  def onOther(exp: OtherExp): Boolean = exp.exp.visit(this)
  def onEpsilon(): Boolean = false
  def onNullSet(): Boolean = false
  def onAnyString(): Boolean = false
  def onData(exp: DataExp): Boolean = false
  def onValue(exp: ValueExp): Boolean = false
}
