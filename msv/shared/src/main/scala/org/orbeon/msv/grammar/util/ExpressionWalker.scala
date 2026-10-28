package org.orbeon.msv.grammar.util

import org.orbeon.msv.grammar.{AttributeExp, BinaryExp, ChoiceExp, ConcurExp, DataExp, ElementExp, ExpressionVisitorVoid, InterleaveExp, ListExp, MixedExp, OneOrMoreExp, OtherExp, ReferenceExp, SequenceExp, ValueExp}

/**
 * Visits all reachable expressions but do nothing.
 *
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
abstract class ExpressionWalker extends ExpressionVisitorVoid {
  def onRef(exp: ReferenceExp): Unit = exp.exp.visit(this)
  def onOther(exp: OtherExp): Unit = exp.exp.visit(this)
  def onElement(exp: ElementExp): Unit = exp.contentModel.visit(this)
  def onEpsilon(): Unit = ()
  def onNullSet(): Unit = ()
  def onAnyString(): Unit = ()
  def onData(exp: DataExp): Unit = ()
  def onValue(exp: ValueExp): Unit = ()
  def onInterleave(exp: InterleaveExp): Unit = onBinExp(exp)
  def onConcur(exp: ConcurExp): Unit = onBinExp(exp)
  def onChoice(exp: ChoiceExp): Unit = onBinExp(exp)
  def onSequence(exp: SequenceExp): Unit = onBinExp(exp)

  def onBinExp(exp: BinaryExp): Unit = {
    exp.exp1.visit(this)
    exp.exp2.visit(this)
  }

  def onMixed(exp: MixedExp): Unit = exp.exp.visit(this)
  def onList(exp: ListExp): Unit = exp.exp.visit(this)
  def onOneOrMore(exp: OneOrMoreExp): Unit = exp.exp.visit(this)
  def onAttribute(exp: AttributeExp): Unit = exp.exp.visit(this)
}
