package org.orbeon.msv.reader.trex

import java.util.{HashMap, HashSet, Map as JMap, Set as JSet}
import org.orbeon.msv.grammar.*

/**
 * makes sure that there is no sequenced string.
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class TREXSequencedStringChecker(
  private val reader: TREXBaseReader,
  private val rejectTextInInterleave: Boolean
) extends ExpressionVisitor:

  // integer pool implementation
  private val intPool: Array[java.lang.Integer] = Array(
    java.lang.Integer.valueOf(0), java.lang.Integer.valueOf(1),
    java.lang.Integer.valueOf(2), java.lang.Integer.valueOf(3),
    java.lang.Integer.valueOf(4), java.lang.Integer.valueOf(5),
    java.lang.Integer.valueOf(6), java.lang.Integer.valueOf(7)
  )

  // 3 bits of information
  private val HAS_ELEMENT = 4
  private val HAS_ANYSTRING = 2
  private val HAS_DATA = 1 // data or value

  private val checkedExps: JSet[Expression] = new HashSet[Expression]()
  private val checkedRefExps: JMap[ReferenceExp, AnyRef] = new HashMap[ReferenceExp, AnyRef]()

  def onRef(exp: ReferenceExp): AnyRef =
    val cached = checkedRefExps.get(exp)
    if cached != null then return cached
    val r = exp.exp.visit(this)
    checkedRefExps.put(exp, r)
    r

  def onOther(exp: OtherExp): AnyRef = exp.exp.visit(this)

  def onInterleave(exp: InterleaveExp): AnyRef =
    val l = exp.exp1.visit(this)
    val r = exp.exp2.visit(this)

    if isError(l, r) then
      reader.reportError(TREXBaseReader.ERR_INTERLEAVED_STRING)
      return intPool(0)

    if rejectTextInInterleave && (toInt(l) & HAS_ANYSTRING) != 0 && (toInt(r) & HAS_ANYSTRING) != 0 then
      reader.reportError(TREXBaseReader.ERR_INTERLEAVED_ANYSTRING)
      return intPool(0)

    merge(l, r)

  def onSequence(exp: SequenceExp): AnyRef =
    val l = exp.exp1.visit(this)
    val r = exp.exp2.visit(this)

    if isError(l, r) then
      reader.reportError(TREXBaseReader.ERR_SEQUENCED_STRING)
      return intPool(0)

    merge(l, r)

  def onEpsilon(): AnyRef = intPool(0)
  def onNullSet(): AnyRef = intPool(0)
  def onData(exp: DataExp): AnyRef = intPool(HAS_DATA)
  def onValue(exp: ValueExp): AnyRef = intPool(HAS_DATA)
  def onList(exp: ListExp): AnyRef = intPool(HAS_DATA)
  def onAnyString(): AnyRef = intPool(HAS_ANYSTRING)

  def onAttribute(exp: AttributeExp): AnyRef =
    if checkedExps.add(exp) then
      exp.exp.visit(this)
    intPool(0)

  def onElement(exp: ElementExp): AnyRef =
    if checkedExps.add(exp) then
      exp.contentModel.visit(this)
    intPool(HAS_ELEMENT)

  private def toInt(o: AnyRef): Int = o.asInstanceOf[java.lang.Integer].intValue()

  private def merge(o1: AnyRef, o2: AnyRef): AnyRef =
    intPool(toInt(o1) | toInt(o2))

  private def isError(o1: AnyRef, o2: AnyRef): Boolean =
    ((toInt(o1) & HAS_DATA) != 0 && toInt(o2) != 0) ||
    ((toInt(o2) & HAS_DATA) != 0 && toInt(o1) != 0)

  def onChoice(exp: ChoiceExp): AnyRef =
    merge(exp.exp1.visit(this), exp.exp2.visit(this))

  def onConcur(exp: ConcurExp): AnyRef =
    merge(exp.exp1.visit(this), exp.exp2.visit(this))

  def onOneOrMore(exp: OneOrMoreExp): AnyRef =
    val o = exp.exp.visit(this)
    if (toInt(o) & HAS_DATA) != 0 then
      reader.reportError(TREXBaseReader.ERR_REPEATED_STRING)
      return intPool(0)
    o

  def onMixed(exp: MixedExp): AnyRef =
    val o = exp.exp.visit(this)
    if rejectTextInInterleave && (toInt(o) & HAS_ANYSTRING) != 0 then
      reader.reportError(TREXBaseReader.ERR_INTERLEAVED_ANYSTRING)
      return intPool(0)
    merge(o, intPool(HAS_ANYSTRING))
