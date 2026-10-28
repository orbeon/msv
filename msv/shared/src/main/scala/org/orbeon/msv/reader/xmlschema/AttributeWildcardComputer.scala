package org.orbeon.msv.reader.xmlschema

import java.util.{ArrayDeque, HashSet, Set}
import org.orbeon.msv.grammar.*
import org.orbeon.msv.grammar.util.ExpressionWalker
import org.orbeon.msv.grammar.xmlschema.{AttWildcardExp, AttributeGroupExp, AttributeWildcard, ComplexTypeExp}
import org.orbeon.msv.util.StringPair

/**
 * Processes the attribute wildcard according to the spec.
 *
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class AttributeWildcardComputer protected (private val reader: XMLSchemaReader) extends ExpressionWalker:

  private val visitedExps: Set[Expression] = new HashSet[Expression]()
  private val unprocessedElementExps = new ArrayDeque[ElementExp]()
  private var wildcards: Set[AttributeWildcard] = null

  private def compute(topLevel: Expression): Unit =
    topLevel.visit(this)
    while !unprocessedElementExps.isEmpty do
      unprocessedElementExps.pop().contentModel.visit(this)

  override def onElement(exp: ElementExp): Unit =
    if !visitedExps.add(exp) then
      return // this element has already been processed
    unprocessedElementExps.add(exp)

  override def onRef(exp: ReferenceExp): Unit =
    if visitedExps.add(exp) then
      exp match
        case aexp: AttributeGroupExp =>
          val o = wildcards
          wildcards = new HashSet[AttributeWildcard]()
          exp.exp.visit(this)
          aexp.wildcard = calcCompleteWildcard(aexp.wildcard, wildcards)
          wildcards = o
        case cexp: ComplexTypeExp =>
          val o = wildcards
          wildcards = new HashSet[AttributeWildcard]()
          exp.exp.visit(this)
          cexp.wildcard = calcCompleteWildcard(cexp.wildcard, wildcards)

          if cexp.complexBaseType != null then
            cexp.complexBaseType.visit(this)
            if cexp.derivationMethod == ComplexTypeExp.EXTENSION then
              cexp.wildcard = calcComplexTypeWildcard(cexp.wildcard, cexp.complexBaseType.wildcard)

            propagateAttributes(cexp)

          if cexp.wildcard != null then
            cexp.attWildcard.exp = cexp.wildcard.createExpression(reader.grammar)

          wildcards = o
        case _ =>
          super.onRef(exp)

    if wildcards != null then
      exp match
        case attWildcardExp: AttWildcardExp =>
          val w = attWildcardExp.getAttributeWildcard
          if w != null then wildcards.add(w)
        case _ =>

  private def calcCompleteWildcard(local: AttributeWildcard, s: Set[AttributeWildcard]): AttributeWildcard =
    val children = s.toArray(new Array[AttributeWildcard](s.size))

    if children.length == 0 then
      return local

    var target = children(0).getName
    var i = 1
    while i < children.length do
      target = NameClass.intersection(target, children(i).getName)
      i += 1

    if local != null then
      new AttributeWildcard(
        NameClass.intersection(local.getName, target),
        local.getProcessMode
      )
    else
      new AttributeWildcard(target, children(0).getProcessMode)

  private def calcComplexTypeWildcard(complete: AttributeWildcard, base: AttributeWildcard): AttributeWildcard =
    if base != null then
      if complete == null then base
      else
        new AttributeWildcard(
          NameClass.union(complete.getName, base.getName),
          complete.getProcessMode
        )
    else
      complete

  private def propagateAttributes(cexp: ComplexTypeExp): Unit =
    if cexp.derivationMethod != ComplexTypeExp.RESTRICTION || cexp.complexBaseType == null then
      return

    if cexp.complexBaseType == reader.complexUrType then
      return

    val explicitAtts = new HashSet[StringPair]()

    cexp.body.visit(new ExpressionWalker:
      override def onElement(exp: ElementExp): Unit = ()
      override def onAttribute(exp: AttributeExp): Unit =
        exp.nameClass match
          case snc: SimpleNameClass =>
            explicitAtts.add(snc.toStringPair())
          case other =>
            throw new Error(other.toString)
    )

    cexp.complexBaseType.body.visit(new ExpressionWalker:
      private var isOptional = false

      override def onChoice(exp: ChoiceExp): Unit =
        val b = isOptional
        isOptional = true
        super.onChoice(exp)
        isOptional = b

      override def onElement(exp: ElementExp): Unit = ()

      override def onAttribute(exp: AttributeExp): Unit =
        exp.nameClass match
          case snc: SimpleNameClass =>
            if !explicitAtts.contains(snc.toStringPair()) then
              cexp.body.exp = reader.pool.createSequence(
                cexp.body.exp,
                if isOptional then reader.pool.createOptional(exp) else exp
              )
          case _ =>
            throw new Error()
    )

object AttributeWildcardComputer:
  def compute(reader: XMLSchemaReader, topLevel: Expression): Unit =
    new AttributeWildcardComputer(reader).compute(topLevel)
