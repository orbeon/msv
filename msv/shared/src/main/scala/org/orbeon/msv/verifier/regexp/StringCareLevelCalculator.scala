package org.orbeon.msv.verifier.regexp

import org.orbeon.msv.datatype.xsd.XSDatatype
import org.orbeon.msv.grammar.*
import org.orbeon.msv.verifier.Acceptor

/**
 * calculates how character literals should be treated.
 *
 * This class is thread-safe: multiple threads can simultaneously
 * access the same instance. Note that there is no guarantee that the
 * derived class is thread-safe.
 *
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class StringCareLevelCalculator protected () extends ExpressionVisitor:
  import StringCareLevelCalculator.*

  def onAttribute(exp: AttributeExp): AnyRef = NO_STRING
  def onElement(exp: ElementExp): AnyRef = NO_STRING
  def onMixed(exp: MixedExp): AnyRef = ANY_STRING
  def onList(exp: ListExp): AnyRef = SOME_STRING
  def onAnyString(): AnyRef = ANY_STRING

  def onData(exp: DataExp): AnyRef =
    if exp.except == Expression.nullSet && exp.dt.isInstanceOf[XSDatatype] then
      val xdt = exp.dt.asInstanceOf[XSDatatype]
      if xdt.isAlwaysValid() then
        return ANY_STRING
    SOME_STRING

  def onValue(exp: ValueExp): AnyRef = SOME_STRING

  def onChoice(exp: ChoiceExp): AnyRef = doChoice(exp)

  private def doChoice(exp: BinaryExp): AnyRef =
    val lhs = exp.exp1.visit(this)
    val rhs = exp.exp2.visit(this)
    if lhs == ANY_STRING && rhs == ANY_STRING then ANY_STRING
    else if lhs == NO_STRING && rhs == NO_STRING then NO_STRING
    else SOME_STRING

  def onOneOrMore(exp: OneOrMoreExp): AnyRef = exp.exp.visit(this)
  def onRef(exp: ReferenceExp): AnyRef = exp.exp.visit(this)
  def onOther(exp: OtherExp): AnyRef = exp.exp.visit(this)
  def onEpsilon(): AnyRef = NO_STRING
  def onNullSet(): AnyRef = NO_STRING

  def onSequence(exp: SequenceExp): AnyRef =
    if !exp.exp1.isEpsilonReducible then
      exp.exp1.visit(this)
    else
      doChoice(exp)

  def onConcur(exp: ConcurExp): AnyRef =
    val lhs = exp.exp1.visit(this)
    val rhs = exp.exp2.visit(this)
    if lhs == ANY_STRING && rhs == ANY_STRING then ANY_STRING
    else if lhs == NO_STRING || rhs == NO_STRING then NO_STRING
    else SOME_STRING

  def onInterleave(p: InterleaveExp): AnyRef = doChoice(p)

object StringCareLevelCalculator:
  val theInstance: StringCareLevelCalculator = new StringCareLevelCalculator()

  private val ANY_STRING: String = "anyString"
  private val NO_STRING: String = "noString"
  private val SOME_STRING: String = "someString"

  def calc(exp: Expression): Int =
    val r = exp.visit(theInstance)
    if r == ANY_STRING then Acceptor.STRING_IGNORE
    else if r == NO_STRING then Acceptor.STRING_PROHIBITED
    else Acceptor.STRING_STRICT
