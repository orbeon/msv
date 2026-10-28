package org.orbeon.msv.verifier.regexp

import java.util.Collection as JCollection
import org.orbeon.msv.grammar.*
import scala.compiletime.uninitialized

/**
 * Collects "elements of concern".
 *
 * "Elements of concern" are ElementExps that are possibly applicable to
 * the next element. These gathered element declarations are then tested against
 * next XML element.
 *
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class ElementsOfConcernCollector extends ExpressionVisitor:

  private var result: JCollection[ElementExp] = uninitialized

  final def collect(exp: Expression, result: JCollection[ElementExp]): Unit =
    this.result = result
    exp.visit(this)

  final def onAttribute(exp: AttributeExp): AnyRef = null

  final def onChoice(exp: ChoiceExp): AnyRef =
    exp.exp1.visit(this)
    exp.exp2.visit(this)
    null

  final def onElement(exp: ElementExp): AnyRef =
    result.add(exp)
    null

  final def onOneOrMore(exp: OneOrMoreExp): AnyRef =
    exp.exp.visit(this)
    null

  final def onMixed(exp: MixedExp): AnyRef =
    exp.exp.visit(this)
    null

  final def onEpsilon(): AnyRef = null
  final def onNullSet(): AnyRef = null
  final def onAnyString(): AnyRef = null
  final def onData(exp: DataExp): AnyRef = null
  final def onValue(exp: ValueExp): AnyRef = null
  final def onList(exp: ListExp): AnyRef = null

  final def onRef(exp: ReferenceExp): AnyRef =
    exp.exp.visit(this)

  final def onOther(exp: OtherExp): AnyRef =
    exp.exp.visit(this)

  final def onSequence(exp: SequenceExp): AnyRef =
    exp.exp1.visit(this)
    if exp.exp1.isEpsilonReducible then
      exp.exp2.visit(this)
    null

  final def onConcur(exp: ConcurExp): AnyRef =
    exp.exp1.visit(this)
    exp.exp2.visit(this)
    null

  final def onInterleave(exp: InterleaveExp): AnyRef =
    exp.exp1.visit(this)
    exp.exp2.visit(this)
    null
