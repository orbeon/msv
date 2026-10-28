package org.orbeon.msv.reader

import java.util.{ArrayDeque, ArrayList as JArrayList, HashSet as JHashSet, List as JList, Set as JSet}
import org.xml.sax.Locator
import org.orbeon.msv.grammar.*

/**
 * makes sure that the expression does not run away.
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@sun.com">Kohsuke KAWAGUCHI</a>
 */
class RunAwayExpressionChecker protected (val reader: GrammarReader) extends ExpressionVisitorVoid:

  private val testedExps: JSet[Expression] = new JHashSet[Expression]()
  private val contentModel: JSet[Expression] = new JHashSet[Expression]()
  private val refStack: JList[Expression] = new JArrayList[Expression]()
  private val unprocessedElementExps: ArrayDeque[ElementExp] = new ArrayDeque[ElementExp]()

  private def check(exp: Expression): Unit =
    try
      exp.visit(this)
      while !unprocessedElementExps.isEmpty do
        contentModel.clear()
        refStack.clear()
        val e = unprocessedElementExps.pop()
        e.contentModel.visit(this)
    catch
      case e: RuntimeException if e eq RunAwayExpressionChecker.eureka =>

  def onAttribute(exp: AttributeExp): Unit =
    enter(exp)
    exp.exp.visit(this)
    leave()

  def onConcur(exp: ConcurExp): Unit = binaryVisit(exp)
  def onInterleave(exp: InterleaveExp): Unit = binaryVisit(exp)
  def onSequence(exp: SequenceExp): Unit = binaryVisit(exp)
  def onChoice(exp: ChoiceExp): Unit = binaryVisit(exp)
  def onOneOrMore(exp: OneOrMoreExp): Unit = unaryVisit(exp)
  def onMixed(exp: MixedExp): Unit = unaryVisit(exp)
  def onList(exp: ListExp): Unit = unaryVisit(exp)
  def onEpsilon(): Unit = ()
  def onNullSet(): Unit = ()
  def onAnyString(): Unit = ()
  def onData(exp: DataExp): Unit = ()
  def onValue(exp: ValueExp): Unit = ()

  protected final def binaryVisit(exp0: BinaryExp): Unit =
    var exp = exp0
    var cnt = 0
    var continue = true
    while continue do
      enter(exp)
      cnt += 1
      exp.exp2.visit(this)
      if exp.exp1.isInstanceOf[BinaryExp] then
        exp = exp.exp1.asInstanceOf[BinaryExp]
      else
        continue = false

    exp.exp1.visit(this)
    while cnt > 0 do
      leave()
      cnt -= 1

  protected final def unaryVisit(exp: UnaryExp): Unit =
    enter(exp)
    exp.exp.visit(this)
    leave()

  private def enter(exp: Expression): Unit =
    if contentModel.contains(exp) then
      var s = ""
      var i = refStack.indexOf(exp)
      val sz = refStack.size()
      val locs = new JArrayList[Locator]()
      while i < sz do
        val item = refStack.get(i)
        if item.isInstanceOf[ReferenceExp] then
          val e = item.asInstanceOf[ReferenceExp]
          if e.name != null then
            if s.length != 0 then s += " > "
            s += e.name
            val loc = reader.getDeclaredLocationOf(e)
            if loc != null then locs.add(loc)
        i += 1
      reader.reportError(locs.toArray(new Array[Locator](locs.size())), GrammarReader.ERR_RUNAWAY_EXPRESSION, Array[AnyRef](s))
      throw RunAwayExpressionChecker.eureka

    contentModel.add(exp)
    refStack.add(exp)

  private def leave(): Unit =
    contentModel.remove(refStack.remove(refStack.size() - 1))

  def onRef(exp: ReferenceExp): Unit =
    enter(exp)
    if !testedExps.contains(exp) then
      testedExps.add(exp)
      exp.exp.visit(this)
    leave()

  def onOther(exp: OtherExp): Unit =
    enter(exp)
    exp.exp.visit(this)
    leave()

  def onElement(exp: ElementExp): Unit =
    if !testedExps.add(exp) then ()
    else unprocessedElementExps.push(exp)

object RunAwayExpressionChecker:
  protected[reader] val eureka: RuntimeException = new RuntimeException()

  def check(reader: GrammarReader, exp: Expression): Unit =
    new RunAwayExpressionChecker(reader).check(exp)
