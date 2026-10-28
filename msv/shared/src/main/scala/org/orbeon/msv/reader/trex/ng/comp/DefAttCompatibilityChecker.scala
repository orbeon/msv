package org.orbeon.msv.reader.trex.ng.comp

import java.util.{HashMap, HashSet, Iterator as JIterator, Map as JMap, Set as JSet}
import org.orbeon.msv.grammar.*
import org.orbeon.msv.grammar.util.{ExpressionWalker, RefExpRemover}
import org.orbeon.msv.relaxng.datatype.Datatype
import org.orbeon.msv.util.StringPair
import org.orbeon.msv.verifier.regexp.{ResidualCalculator, StringToken}
import org.xml.sax.Locator

class DefAttCompatibilityChecker(
  reader: RELAXNGCompReader,
  private val defaultedAttributes: JMap[AttributeExp, String]
) extends CompatibilityChecker(reader):

  protected def setCompatibility(val_ : Boolean): Unit =
    grammar.isDefaultAttributeValueCompatible = val_

  private final class AbortException extends RuntimeException

  private final class DefAttMap(val sampleDecl: ElementExp, val defaultAttributes: JMap[StringPair, String])

  private val refRemover: RefExpRemover = new RefExpRemover(reader.pool, false)

  private def isEpsilon(exp: Expression): Boolean =
    if exp == Expression.epsilon then true
    else exp.visit(refRemover) == Expression.epsilon

  def test(): Unit =
    grammar.isDefaultAttributeValueCompatible = true

    if defaultedAttributes.isEmpty then return

    val resCalc = new ResidualCalculator(reader.pool)
    val itr = defaultedAttributes.entrySet().iterator()

    while itr.hasNext do
      val item = itr.next()
      val exp = item.getKey
      val value = item.getValue

      // tests if the name class is simple
      if !exp.nameClass.isInstanceOf[SimpleNameClass] then
        reportCompError(
          Array[Locator](reader.getDeclaredLocationOf(exp)),
          DefAttCompatibilityChecker.CERR_DEFVALUE_NAME_IS_NOT_SIMPLE
        )

      // tests if there is no context-dependent datatypes
      try
        exp.exp.visit(contextDependentTypeChecker)
      catch
        case _: AbortException =>
          // skip further check, error already reported

      // tests if the default value matches the content model of this attribute
      val token = new StringToken(resCalc, value, null.asInstanceOf[org.orbeon.msv.grammar.IDContextProvider2])
      if !resCalc.calcResidual(exp.exp, token).isEpsilonReducible then
        reportCompError(
          Array[Locator](reader.getDeclaredLocationOf(exp)),
          DefAttCompatibilityChecker.CERR_DEFVALUE_INVALID,
          Array[AnyRef](value)
        )

    if !grammar.isDefaultAttributeValueCompatible then return

    val name2value: JMap[StringPair, DefAttMap] = new HashMap[StringPair, DefAttMap]()
    val elements: JSet[ElementExp] = new HashSet[ElementExp]()

    grammar.visit(new ExpressionWalker {
      private var inOneOrMore: Boolean = false
      private var inChoice: Boolean = false
      private var inOptionalChoice: Boolean = false
      private var inSimpleElement: Boolean = false
      private var currentAttributes: JMap[StringPair, String] = null
      private var currentElementName: SimpleNameClass = null

      override def onElement(exp: ElementExp): Unit =
        if !elements.add(exp) then return

        val oldSE = inSimpleElement
        val oldOC = inOptionalChoice
        val oldC = inChoice
        val oldOOM = inOneOrMore
        val prevElemName = currentElementName
        val oldCA = currentAttributes

        inSimpleElement = exp.getNameClass.isInstanceOf[SimpleNameClass]
        inOptionalChoice = true
        inChoice = false
        inOneOrMore = false

        var en: StringPair = null
        if inSimpleElement then
          currentElementName = exp.getNameClass.asInstanceOf[SimpleNameClass]
          en = new StringPair(currentElementName)
          currentAttributes = new HashMap[StringPair, String]()
        else
          currentElementName = null

        exp.contentModel.visit(this)

        if en != null then
          val m = name2value.get(en)
          if m == null then
            name2value.put(en, new DefAttMap(exp, currentAttributes))
          else if m.defaultAttributes != currentAttributes then
            reportCompError(
              Array[Locator](
                reader.getDeclaredLocationOf(m.sampleDecl),
                reader.getDeclaredLocationOf(exp)
              ),
              DefAttCompatibilityChecker.CERR_DEFVALUE_COMPETING_ELEMENTS,
              Array[AnyRef](m.sampleDecl.getNameClass.asInstanceOf[SimpleNameClass].localName)
            )
            name2value.remove(en)

        inSimpleElement = oldSE
        inOptionalChoice = oldOC
        inChoice = oldC
        inOneOrMore = oldOOM
        currentElementName = prevElemName
        currentAttributes = oldCA

      override def onOneOrMore(exp: OneOrMoreExp): Unit =
        val oldOOM = inOneOrMore
        inOneOrMore = true
        exp.exp.visit(this)
        inOneOrMore = oldOOM

      override def onChoice(exp: ChoiceExp): Unit =
        val oldOC = inOptionalChoice
        val oldC = inChoice

        inChoice = true
        if !isEpsilon(exp.exp1) && !isEpsilon(exp.exp2) then
          inOptionalChoice = false
        super.onChoice(exp)

        inOptionalChoice = oldOC
        inChoice = oldC

      override def onAttribute(exp: AttributeExp): Unit =
        if defaultedAttributes.containsKey(exp) then
          if !inOptionalChoice || !inChoice then
            reportCompError(
              Array[Locator](reader.getDeclaredLocationOf(exp)),
              DefAttCompatibilityChecker.CERR_DEFVALUE_NOT_OPTIONAL
            )
            return
          if inOneOrMore then
            reportCompError(
              Array[Locator](reader.getDeclaredLocationOf(exp)),
              DefAttCompatibilityChecker.CERR_DEFVALUE_REPEATABLE
            )
            return
          if !inSimpleElement then
            reportCompError(
              Array[Locator](reader.getDeclaredLocationOf(exp)),
              DefAttCompatibilityChecker.CERR_DEFVALUE_COMPLEX_ELEMENTNAME
            )
            return

          val value = defaultedAttributes.get(exp)
          val v = currentAttributes.put(new StringPair(exp.nameClass.asInstanceOf[SimpleNameClass]), value)
          if v != null && v != value then
            reportCompError(
              Array[Locator](reader.getDeclaredLocationOf(exp)),
              DefAttCompatibilityChecker.CERR_DEFVALUE_DIFFERENT_VALUES,
              Array[AnyRef](v, value, currentElementName.localName, exp.nameClass.asInstanceOf[SimpleNameClass].localName)
            )

      override def onList(exp: ListExp): Unit = ()
    })

    val elemItr = elements.iterator()
    while elemItr.hasNext do
      val eexp = elemItr.next()
      val nc = eexp.getNameClass
      if !nc.isInstanceOf[SimpleNameClass] then
        val jtr = name2value.entrySet().iterator()
        while jtr.hasNext do
          val e = jtr.next()
          if nc.accepts(e.getKey) then
            val defAtts = e.getValue
            if !defAtts.defaultAttributes.isEmpty then
              reportCompError(
                Array[Locator](
                  reader.getDeclaredLocationOf(defAtts.sampleDecl),
                  reader.getDeclaredLocationOf(eexp)
                ),
                DefAttCompatibilityChecker.CERR_DEFVALUE_COMPETING_ELEMENTS,
                Array[AnyRef](defAtts.sampleDecl.getNameClass.asInstanceOf[SimpleNameClass].localName)
              )
              return

  private val contextDependentTypeChecker: ExpressionWalker = new ExpressionWalker {
    override def onData(exp: DataExp): Unit = check(exp.dt, exp.name)
    override def onValue(exp: ValueExp): Unit = check(exp.dt, exp.name)
    private def check(dt: Datatype, name: StringPair): Unit =
      if dt.isContextDependent() then
        reportCompError(
          null,
          DefAttCompatibilityChecker.CERR_DEFVALUE_CONTEXT_DEPENDENT_TYPE,
          Array[AnyRef](name.localName)
        )
        throw new AbortException()
  }

object DefAttCompatibilityChecker:
  val CERR_DEFVALUE_NAME_IS_NOT_SIMPLE = "RELAXNGReader.Compatibility.DefaultValue.NameIsNotSimple"
  val CERR_DEFVALUE_INVALID = "RELAXNGReader.Compatibility.DefaultValue.Invalid"
  val CERR_DEFVALUE_NOT_OPTIONAL = "RELAXNGReader.Compatibility.DefaultValue.NotOptional"
  val CERR_DEFVALUE_REPEATABLE = "RELAXNGReader.Compatibility.DefaultValue.Repeatable"
  val CERR_DEFVALUE_COMPLEX_ELEMENTNAME = "RELAXNGReader.Compatibility.DefaultValue.ComplexElementName"
  val CERR_DEFVALUE_DIFFERENT_VALUES = "RELAXNGReader.Compatibility.DefaultValue.DifferentValues"
  val CERR_DEFVALUE_CONTEXT_DEPENDENT_TYPE = "RELAXNGReader.Compatibility.DefaultValue.ContextDependentType"
  val CERR_DEFVALUE_COMPETING_ELEMENTS = "RELAXNGReader.Compatibility.DefaultValue.CompetingElements"
