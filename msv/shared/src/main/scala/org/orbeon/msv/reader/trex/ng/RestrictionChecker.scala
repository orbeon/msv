package org.orbeon.msv.reader.trex.ng

import java.util.HashSet
import org.orbeon.msv.grammar.*
import org.orbeon.msv.grammar.util.{ExpressionWalker, NameClassCollisionChecker}
import org.xml.sax.Locator

/**
 * Checks RELAX NG contextual restrictions defined in section 7.
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class RestrictionChecker(private val reader: RELAXNGReader):

  def check(): Unit =
    reader.getGrammar.visit(inStart)

  private var errorContext: Expression = null

  private def reportError(exp: Expression, errorMsg: String): Unit =
    reportError(exp, errorMsg, null)

  private def reportError(exp: Expression, errorMsg: String, args: Array[AnyRef]): Unit =
    reader.reportError(
      Array[Locator](
        reader.getDeclaredLocationOf(exp),
        reader.getDeclaredLocationOf(errorContext)
      ),
      errorMsg,
      args
    )

  private val visitedExps: java.util.Set[Expression] = new HashSet[Expression]()
  private var attDupChecker: DuplicateAttributesChecker = null
  private var elemDupChecker: DuplicateElementsChecker = null

  private class DefaultChecker extends ExpressionWalker:
    override def onElement(exp: ElementExp): Unit =
      if !visitedExps.add(exp) then return

      if elemDupChecker != null then
        elemDupChecker.add(exp)

      val oldContext = errorContext
      val oldADC = attDupChecker
      val oldEDC = elemDupChecker

      errorContext = exp
      attDupChecker = new DuplicateAttributesChecker()
      elemDupChecker = new DuplicateElementsChecker()

      exp.contentModel.getExpandedExp(reader.pool).visit(inElement)
      errorContext = oldContext
      attDupChecker = oldADC
      elemDupChecker = oldEDC

    override def onAttribute(exp: AttributeExp): Unit =
      if !visitedExps.add(exp) then return

      if attDupChecker != null then
        attDupChecker.add(exp)

      checkAttributeInfiniteName(exp)

      val oldContext = errorContext
      errorContext = exp

      exp.exp.getExpandedExp(reader.pool).visit(inAttribute)
      errorContext = oldContext

    protected def checkAttributeInfiniteName(exp: AttributeExp): Unit =
      exp.nameClass.visit(new NameClassVisitor {
        def onAnyName(nc: AnyNameClass): AnyRef = error()
        def onSimple(nc: SimpleNameClass): AnyRef = null
        def onNsName(nc: NamespaceNameClass): AnyRef = error()
        def onNot(nc: NotNameClass): AnyRef = throw new Error()
        def onDifference(nc: DifferenceNameClass): AnyRef =
          nc.nc1.visit(this)
          nc.nc2.visit(this)
          null
        def onChoice(nc: ChoiceNameClass): AnyRef =
          nc.nc1.visit(this)
          nc.nc2.visit(this)
          null
        private def error(): AnyRef =
          reportError(exp, RELAXNGReader.ERR_NAKED_INFINITE_ATTRIBUTE_NAMECLASS)
          null
      })

    override def onList(exp: ListExp): Unit =
      exp.exp.visit(inList)

    override def onData(exp: DataExp): Unit =
      exp.except.visit(inExcept)

    override def onChoice(exp: ChoiceExp): Unit =
      if attDupChecker == null then
        super.onChoice(exp)
      else
        val idx = attDupChecker.start()
        exp.exp1.visit(this)
        attDupChecker.endLeftBranch(idx)
        exp.exp2.visit(this)
        attDupChecker.endRightBranch()

    override def onInterleave(exp: InterleaveExp): Unit =
      if elemDupChecker == null then
        super.onInterleave(exp)
      else
        val idx = elemDupChecker.start()
        exp.exp1.visit(this)
        elemDupChecker.endLeftBranch(idx)
        exp.exp2.visit(this)
        elemDupChecker.endRightBranch()

    override def onAnyString(): Unit =
      super.onAnyString()

  private val inExcept: ExpressionWalker = new DefaultChecker {
    override def onAttribute(exp: AttributeExp): Unit = reportError(exp, RestrictionChecker.ERR_ATTRIBUTE_IN_EXCEPT)
    override def onElement(exp: ElementExp): Unit = reportError(exp, RestrictionChecker.ERR_ELEMENT_IN_EXCEPT)
    override def onList(exp: ListExp): Unit = reportError(exp, RestrictionChecker.ERR_LIST_IN_EXCEPT)
    override def onAnyString(): Unit = reportError(null, RestrictionChecker.ERR_TEXT_IN_EXCEPT)
    override def onEpsilon(): Unit = reportError(null, RestrictionChecker.ERR_EMPTY_IN_EXCEPT)
    override def onSequence(exp: SequenceExp): Unit = reportError(exp, RestrictionChecker.ERR_SEQUENCE_IN_EXCEPT)
    override def onInterleave(exp: InterleaveExp): Unit = reportError(exp, RestrictionChecker.ERR_INTERLEAVE_IN_EXCEPT)
    override def onOneOrMore(exp: OneOrMoreExp): Unit = reportError(exp, RestrictionChecker.ERR_ONEORMORE_IN_EXCEPT)
  }

  private val inGroupInOneOrMoreInElement: ExpressionWalker = new DefaultChecker {
    override def onAttribute(exp: AttributeExp): Unit = reportError(exp, RestrictionChecker.ERR_REPEATED_GROUPED_ATTRIBUTE)
  }

  private val inOneOrMoreInElement: ExpressionWalker = new DefaultChecker {
    override def onSequence(exp: SequenceExp): Unit = exp.visit(inGroupInOneOrMoreInElement)
    override def onInterleave(exp: InterleaveExp): Unit = exp.visit(inGroupInOneOrMoreInElement)
    override protected def checkAttributeInfiniteName(exp: AttributeExp): Unit = ()
  }

  private val inElement: ExpressionWalker = new DefaultChecker {
    override def onOneOrMore(exp: OneOrMoreExp): Unit = exp.exp.visit(inOneOrMoreInElement)
  }

  private val inAttribute: ExpressionWalker = new DefaultChecker {
    override def onElement(exp: ElementExp): Unit = reportError(exp, RestrictionChecker.ERR_ELEMENT_IN_ATTRIBUTE)
    override def onAttribute(exp: AttributeExp): Unit = reportError(exp, RestrictionChecker.ERR_ATTRIBUTE_IN_ATTRIBUTE)
  }

  private class ListChecker extends DefaultChecker:
    override def onAttribute(exp: AttributeExp): Unit = reportError(exp, RestrictionChecker.ERR_ATTRIBUTE_IN_LIST)
    override def onElement(exp: ElementExp): Unit = reportError(exp, RestrictionChecker.ERR_ELEMENT_IN_LIST)
    override def onList(exp: ListExp): Unit = reportError(exp, RestrictionChecker.ERR_LIST_IN_LIST)
    override def onAnyString(): Unit = reportError(null, RestrictionChecker.ERR_TEXT_IN_LIST)

  private val inInterleaveInList: ExpressionWalker = new ListChecker {
    override def onData(exp: DataExp): Unit = reportError(exp, RestrictionChecker.ERR_DATA_IN_INTERLEAVE_IN_LIST)
    override def onValue(exp: ValueExp): Unit = reportError(exp, RestrictionChecker.ERR_VALUE_IN_INTERLEAVE_IN_LIST)
  }

  private val inList: ExpressionWalker = new ListChecker {
    override def onInterleave(exp: InterleaveExp): Unit = inInterleaveInList.onInterleave(exp)
  }

  private val inStart: ExpressionWalker = new DefaultChecker {
    override def onAttribute(exp: AttributeExp): Unit = reportError(exp, RestrictionChecker.ERR_ATTRIBUTE_IN_START)
    override def onList(exp: ListExp): Unit = reportError(exp, RestrictionChecker.ERR_LIST_IN_START)
    override def onAnyString(): Unit = reportError(null, RestrictionChecker.ERR_TEXT_IN_START)
    override def onEpsilon(): Unit = reportError(null, RestrictionChecker.ERR_EMPTY_IN_START)
    override def onSequence(exp: SequenceExp): Unit = reportError(exp, RestrictionChecker.ERR_SEQUENCE_IN_START)
    override def onInterleave(exp: InterleaveExp): Unit = reportError(exp, RestrictionChecker.ERR_INTERLEAVE_IN_START)
    override def onData(exp: DataExp): Unit = reportError(exp, RestrictionChecker.ERR_DATA_IN_START)
    override def onValue(exp: ValueExp): Unit = reportError(exp, RestrictionChecker.ERR_DATA_IN_START)
    override def onOneOrMore(exp: OneOrMoreExp): Unit = reportError(exp, RestrictionChecker.ERR_ONEORMORE_IN_START)
  }

  class NameClassWalker extends NameClassVisitor:
    def onAnyName(nc: AnyNameClass): AnyRef = null
    def onSimple(nc: SimpleNameClass): AnyRef = null
    def onNsName(nc: NamespaceNameClass): AnyRef = null
    def onNot(nc: NotNameClass): AnyRef = throw new Error()
    def onDifference(nc: DifferenceNameClass): AnyRef =
      nc.nc1.visit(this)
      if nc.nc1.isInstanceOf[AnyNameClass] then
        nc.nc2.visit(inAnyNameClass)
      else if nc.nc1.isInstanceOf[NamespaceNameClass] then
        nc.nc2.visit(inNsNameClass)
      else
        throw new Error()
      null
    def onChoice(nc: ChoiceNameClass): AnyRef =
      nc.nc1.visit(this)
      nc.nc2.visit(this)
      null

  def checkNameClass(nc: NameClass): Unit =
    nc.visit(inNameClass)

  private val inNameClass: NameClassWalker = new NameClassWalker()

  private val inAnyNameClass: NameClassVisitor = new NameClassWalker {
    override def onAnyName(nc: AnyNameClass): AnyRef =
      reportError(null, RestrictionChecker.ERR_ANYNAME_IN_ANYNAME)
      null
  }

  private val inNsNameClass: NameClassVisitor = new NameClassWalker {
    override def onAnyName(nc: AnyNameClass): AnyRef =
      reportError(null, RestrictionChecker.ERR_ANYNAME_IN_NSNAME)
      null
    override def onNsName(nc: NamespaceNameClass): AnyRef =
      reportError(null, RestrictionChecker.ERR_NSNAME_IN_NSNAME)
      null
  }

  protected abstract class DuplicateNameChecker:
    protected var exps: Array[NameClassAndExpression] = new Array[NameClassAndExpression](16)
    protected var expsLen: Int = 0

    protected var areas: Array[Int] = new Array[Int](8)
    protected var areaLen: Int = 0

    def add(exp: NameClassAndExpression): Unit =
      check(exp)
      if exps.length == expsLen then
        val n = new Array[NameClassAndExpression](expsLen * 2)
        System.arraycopy(exps, 0, n, 0, expsLen)
        exps = n
      exps(expsLen) = exp
      expsLen += 1

    protected def check(exp: NameClassAndExpression): Unit

    def start(): Int = expsLen

    def endLeftBranch(start: Int): Unit =
      if areas.length == areaLen then
        val n = new Array[Int](areaLen * 2)
        System.arraycopy(areas, 0, n, 0, areaLen)
        areas = n
      areas(areaLen) = start
      areaLen += 1
      areas(areaLen) = expsLen
      areaLen += 1

    def endRightBranch(): Unit =
      areaLen -= 2

    private val checker = new NameClassCollisionChecker()

    protected def check(newExp: NameClassAndExpression, oldExp: NameClassAndExpression): Unit =
      if checker.check(newExp.getNameClass, oldExp.getNameClass) then
        val intersection = NameClass.intersection(newExp.getNameClass, oldExp.getNameClass)
        reader.reportError(
          Array[Locator](
            reader.getDeclaredLocationOf(errorContext),
            reader.getDeclaredLocationOf(newExp),
            reader.getDeclaredLocationOf(oldExp)
          ),
          getErrorMessage(),
          Array[AnyRef](intersection.toString)
        )

    protected def getErrorMessage(): String

  private class DuplicateElementsChecker extends DuplicateNameChecker:
    protected def check(exp: NameClassAndExpression): Unit =
      var i = 0
      while i < areaLen do
        var j = areas(i)
        val end = areas(i + 1)
        while j < end do
          this.check(exp, exps(j))
          j += 1
        i += 2

    protected def getErrorMessage(): String = RestrictionChecker.ERR_DUPLICATE_ELEMENTS

  private class DuplicateAttributesChecker extends DuplicateNameChecker:
    protected def check(exp: NameClassAndExpression): Unit =
      var j = 0
      var i = 0
      while i < areaLen do
        while j < areas(i) do
          this.check(exp, exps(j))
          j += 1
        j = areas(i + 1)
        i += 2
      while j < expsLen do
        this.check(exp, exps(j))
        j += 1

    protected def getErrorMessage(): String = RestrictionChecker.ERR_DUPLICATE_ATTRIBUTES

object RestrictionChecker:
  val ERR_ATTRIBUTE_IN_EXCEPT = "RELAXNGReader.AttributeInExcept"
  val ERR_ELEMENT_IN_EXCEPT = "RELAXNGReader.ElementInExcept"
  val ERR_LIST_IN_EXCEPT = "RELAXNGReader.ListInExcept"
  val ERR_TEXT_IN_EXCEPT = "RELAXNGReader.TextInExcept"
  val ERR_EMPTY_IN_EXCEPT = "RELAXNGReader.EmptyInExcept"
  val ERR_SEQUENCE_IN_EXCEPT = "RELAXNGReader.SequenceInExcept"
  val ERR_INTERLEAVE_IN_EXCEPT = "RELAXNGReader.InterleaveInExcept"
  val ERR_ONEORMORE_IN_EXCEPT = "RELAXNGReader.OneOrMoreInExcept"
  val ERR_REPEATED_GROUPED_ATTRIBUTE = "RELAXNGReader.RepeatedGroupedAttribute"
  val ERR_ELEMENT_IN_ATTRIBUTE = "RELAXNGReader.ElementInAttribute"
  val ERR_ATTRIBUTE_IN_ATTRIBUTE = "RELAXNGReader.AttributeInAttribute"
  val ERR_ATTRIBUTE_IN_LIST = "RELAXNGReader.AttributeInList"
  val ERR_ELEMENT_IN_LIST = "RELAXNGReader.ElementInList"
  val ERR_LIST_IN_LIST = "RELAXNGReader.ListInList"
  val ERR_TEXT_IN_LIST = "RELAXNGReader.TextInList"
  val ERR_ATTRIBUTE_IN_START = "RELAXNGReader.AttributeInStart"
  val ERR_LIST_IN_START = "RELAXNGReader.ListInStart"
  val ERR_TEXT_IN_START = "RELAXNGReader.TextInStart"
  val ERR_EMPTY_IN_START = "RELAXNGReader.EmptyInStart"
  val ERR_SEQUENCE_IN_START = "RELAXNGReader.SequenceInStart"
  val ERR_INTERLEAVE_IN_START = "RELAXNGReader.InterleaveInStart"
  val ERR_DATA_IN_START = "RELAXNGReader.DataInStart"
  val ERR_ONEORMORE_IN_START = "RELAXNGReader.OneOrMoreInStart"
  val ERR_DATA_IN_INTERLEAVE_IN_LIST = "RELAXNGReader.DataInInterleaveInList"
  val ERR_VALUE_IN_INTERLEAVE_IN_LIST = "RELAXNGReader.ValueInInterleaveInList"
  val ERR_ANYNAME_IN_ANYNAME = "RELAXNGReader.AnyNameInAnyName"
  val ERR_ANYNAME_IN_NSNAME = "RELAXNGReader.AnyNameInNsName"
  val ERR_NSNAME_IN_NSNAME = "RELAXNGReader.NsNameInNsName"
  val ERR_DUPLICATE_ATTRIBUTES = "RELAXNGReader.DuplicateAttributes"
  val ERR_DUPLICATE_ELEMENTS = "RELAXNGReader.DuplicateElements"
