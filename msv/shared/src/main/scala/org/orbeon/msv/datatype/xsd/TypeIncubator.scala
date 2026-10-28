/*
 * @(#)$Id: TypeIncubator.java,v 1.30 2005/03/31 23:19:10 kohsuke Exp $
 *
 * Copyright 2001 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * This software is the proprietary information of Sun Microsystems, Inc.  
 * Use is subject to license terms.
 * 
 */
package org.orbeon.msv.datatype.xsd

import org.orbeon.msv.relaxng.datatype.{DatatypeException, ValidationContext}

import java.math.BigInteger
import java.util.{ArrayList, HashMap, Map => JMap}

/**
 * Derives a new type by adding facets.
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class TypeIncubator(baseType0: XSDatatype):

  if baseType0 == null then
    throw new IllegalArgumentException()

  private val baseType: XSDatatypeImpl = baseType0.asInstanceOf[XSDatatypeImpl]
  private val impl: JMap[String, FacetInfo] = new HashMap[String, FacetInfo]()

  @deprecated("please use addFacet", "")
  @throws(classOf[DatatypeException])
  def add(name: String, strValue: String, fixed: Boolean, context: ValidationContext): Unit =
    addFacet(name, strValue, fixed, context)

  @throws(classOf[DatatypeException])
  def addFacet(name: String, strValue: String, fixed: Boolean, context: ValidationContext): Unit =
    if baseType.isInstanceOf[ErrorType] then
      return // silently ignore any further error

    // checks applicability of the facet
    baseType.isFacetApplicable(name) match
      case XSDatatype.APPLICABLE => ()
      case XSDatatype.FIXED      => return // simply ignore this facet
      case XSDatatype.NOT_ALLOWED =>
        throw new DatatypeException(XSDatatypeImpl.localize(XSDatatypeImpl.ERR_NOT_APPLICABLE_FACET, name))
      case _ =>
        throw new Error()

    val value: AnyRef =
      if TypeIncubator.isValueFacet(name) then
        val v = baseType.createValue(strValue, context)
        if v == null then
          throw new DatatypeException(
            XSDatatypeImpl.localize(
              XSDatatypeImpl.ERR_INVALID_VALUE_FOR_THIS_TYPE,
              strValue,
              baseType.displayName()
            )
          )
        v
      else
        strValue

    if TypeIncubator.isRepeatable(name) then
      var fi = impl.get(name)
      if fi == null then
        fi = new FacetInfo(new ArrayList[AnyRef](), fixed)
        impl.put(name, fi)
      fi.value.asInstanceOf[ArrayList[AnyRef]].add(value)
      fi.fixed = fi.fixed || fixed
    else
      if impl.containsKey(name) then
        throw new DatatypeException(XSDatatypeImpl.localize(XSDatatypeImpl.ERR_DUPLICATE_FACET, name))
      impl.put(name, new FacetInfo(value, fixed))

  @deprecated("", "")
  @throws(classOf[DatatypeException])
  def derive(newName: String): XSDatatypeImpl =
    derive("", newName)

  @throws(classOf[DatatypeException])
  def derive(newNameUri: String, newLocalName: String): XSDatatypeImpl =
    if baseType.isInstanceOf[ErrorType] then
      return baseType

    if baseType.isFinal(XSDatatype.DERIVATION_BY_RESTRICTION) then
      throw new DatatypeException(
        XSDatatypeImpl.localize(XSDatatypeImpl.ERR_INVALID_BASE_TYPE, baseType.displayName())
      )

    if isEmpty() then
      if newNameUri == null && newLocalName == null then
        return baseType
      return new FinalComponent(newNameUri, newLocalName, baseType, 0)

    var r: XSDatatypeImpl = baseType

    var i = 0
    while i < TypeIncubator.exclusiveFacetPairs.length do
      val pair = TypeIncubator.exclusiveFacetPairs(i)
      if contains(pair(0)) && contains(pair(1)) then
        throw new DatatypeException(
          XSDatatypeImpl.localize(
            XSDatatypeImpl.ERR_X_AND_Y_ARE_EXCLUSIVE,
            pair(0),
            pair(1)
          )
        )
      i += 1

    if contains(XSDatatype.FACET_TOTALDIGITS) then
      r = new TotalDigitsFacet(
        newNameUri,
        newLocalName,
        r,
        getPositiveInteger(XSDatatype.FACET_TOTALDIGITS),
        isFixed(XSDatatype.FACET_TOTALDIGITS)
      )
    if contains(XSDatatype.FACET_FRACTIONDIGITS) then
      r = new FractionDigitsFacet(
        newNameUri,
        newLocalName,
        r,
        getNonNegativeInteger(XSDatatype.FACET_FRACTIONDIGITS),
        isFixed(XSDatatype.FACET_FRACTIONDIGITS)
      )
    if contains(XSDatatype.FACET_MININCLUSIVE) then
      r = new MinInclusiveFacet(
        newNameUri,
        newLocalName,
        r,
        getFacet(XSDatatype.FACET_MININCLUSIVE),
        isFixed(XSDatatype.FACET_MININCLUSIVE)
      )
    if contains(XSDatatype.FACET_MAXINCLUSIVE) then
      r = new MaxInclusiveFacet(
        newNameUri,
        newLocalName,
        r,
        getFacet(XSDatatype.FACET_MAXINCLUSIVE),
        isFixed(XSDatatype.FACET_MAXINCLUSIVE)
      )
    if contains(XSDatatype.FACET_MINEXCLUSIVE) then
      r = new MinExclusiveFacet(
        newNameUri,
        newLocalName,
        r,
        getFacet(XSDatatype.FACET_MINEXCLUSIVE),
        isFixed(XSDatatype.FACET_MINEXCLUSIVE)
      )
    if contains(XSDatatype.FACET_MAXEXCLUSIVE) then
      r = new MaxExclusiveFacet(
        newNameUri,
        newLocalName,
        r,
        getFacet(XSDatatype.FACET_MAXEXCLUSIVE),
        isFixed(XSDatatype.FACET_MAXEXCLUSIVE)
      )
    if contains(XSDatatype.FACET_LENGTH) then
      r = new LengthFacet(newNameUri, newLocalName, r, this)
    if contains(XSDatatype.FACET_MINLENGTH) then
      r = new MinLengthFacet(newNameUri, newLocalName, r, this)
    if contains(XSDatatype.FACET_MAXLENGTH) then
      r = new MaxLengthFacet(newNameUri, newLocalName, r, this)
    if contains(XSDatatype.FACET_WHITESPACE) then
      r = new WhiteSpaceFacet(newNameUri, newLocalName, r, this)
    if contains(XSDatatype.FACET_PATTERN) then
      r = new PatternFacet(newNameUri, newLocalName, r, this)
    if contains(XSDatatype.FACET_ENUMERATION) then
      r = new EnumerationFacet(
        newNameUri,
        newLocalName,
        r,
        getVector(XSDatatype.FACET_ENUMERATION),
        isFixed(XSDatatype.FACET_ENUMERATION)
      )

    // additional facet consistency check
    val o1Length = r.getFacetObject(XSDatatype.FACET_MAXLENGTH)
    val o2Length = r.getFacetObject(XSDatatype.FACET_MINLENGTH)
    if o1Length != null && o2Length != null &&
       o1Length.asInstanceOf[MaxLengthFacet].maxLength < o2Length.asInstanceOf[MinLengthFacet].minLength then
      throw TypeIncubator.reportFacetInconsistency(
        newLocalName,
        o1Length,
        XSDatatype.FACET_MAXLENGTH,
        o2Length,
        XSDatatype.FACET_MINLENGTH
      )

    val o1Digits = r.getFacetObject(XSDatatype.FACET_FRACTIONDIGITS)
    val o2Digits = r.getFacetObject(XSDatatype.FACET_TOTALDIGITS)
    if o1Digits != null && o2Digits != null &&
       o1Digits.asInstanceOf[FractionDigitsFacet].scale > o2Digits.asInstanceOf[TotalDigitsFacet].precision then
      throw TypeIncubator.reportFacetInconsistency(
        newLocalName,
        o1Digits,
        XSDatatype.FACET_FRACTIONDIGITS,
        o2Digits,
        XSDatatype.FACET_TOTALDIGITS
      )

    TypeIncubator.checkRangeConsistency(r, XSDatatype.FACET_MININCLUSIVE, XSDatatype.FACET_MAXINCLUSIVE)
    TypeIncubator.checkRangeConsistency(r, XSDatatype.FACET_MINEXCLUSIVE, XSDatatype.FACET_MAXEXCLUSIVE)
    TypeIncubator.checkRangeConsistency(r, XSDatatype.FACET_MININCLUSIVE, XSDatatype.FACET_MAXEXCLUSIVE)
    TypeIncubator.checkRangeConsistency(r, XSDatatype.FACET_MINEXCLUSIVE, XSDatatype.FACET_MAXINCLUSIVE)

    r

  def isFixed(facetName: String): Boolean =
    impl.get(facetName).fixed

  def getFacet(facetName: String): AnyRef =
    impl.get(facetName).value

  def getVector(facetName: String): ArrayList[AnyRef] =
    impl.get(facetName).value.asInstanceOf[ArrayList[AnyRef]]

  @throws(classOf[DatatypeException])
  def getPositiveInteger(facetName: String): Int =
    try
      val value = Integer.parseInt(getFacet(facetName).asInstanceOf[String])
      if value > 0 then return value
    catch
      case _: NumberFormatException =>
        try
          if new BigInteger(getFacet(facetName).asInstanceOf[String]).signum() > 0 then
            return Integer.MAX_VALUE
        catch
          case _: NumberFormatException => ()

    throw new DatatypeException(
      XSDatatypeImpl.localize(XSDatatypeImpl.ERR_FACET_MUST_BE_POSITIVE_INTEGER, facetName)
    )

  @throws(classOf[DatatypeException])
  def getNonNegativeInteger(facetName: String): Int =
    try
      val value = Integer.parseInt(getFacet(facetName).asInstanceOf[String])
      if value >= 0 then return value
    catch
      case _: NumberFormatException => ()

    throw new DatatypeException(
      XSDatatypeImpl.localize(XSDatatypeImpl.ERR_FACET_MUST_BE_NON_NEGATIVE_INTEGER, facetName)
    )

  private def contains(facetName: String): Boolean =
    impl.containsKey(facetName)

  def isEmpty(): Boolean =
    impl.isEmpty

  def dump(out: java.io.PrintStream): Unit =
    val itr = impl.keySet().iterator()
    while itr.hasNext do
      val facetName = itr.next()
      val fi = impl.get(facetName)
      fi.value match
        case v: ArrayList[?] =>
          out.println(facetName + " :")
          var i = 0
          while i < v.size() do
            out.println("  " + v.get(i))
            i += 1
        case other =>
          out.println(facetName + " : " + other)

  def getFacetNames(): String =
    var r = ""
    val itr = impl.keySet().iterator()
    while itr.hasNext do
      if r.length != 0 then r += ", "
      r += itr.next()
    r

private class FacetInfo(var value: AnyRef, var fixed: Boolean)

object TypeIncubator:

  val exclusiveFacetPairs: Array[Array[String]] = Array(
    Array(XSDatatype.FACET_LENGTH, XSDatatype.FACET_MINLENGTH),
    Array(XSDatatype.FACET_LENGTH, XSDatatype.FACET_MAXLENGTH),
    Array(XSDatatype.FACET_MAXINCLUSIVE, XSDatatype.FACET_MAXEXCLUSIVE),
    Array(XSDatatype.FACET_MININCLUSIVE, XSDatatype.FACET_MINEXCLUSIVE)
  )

  @throws(classOf[DatatypeException])
  private def checkRangeConsistency(newType: XSDatatypeImpl, facetName1: String, facetName2: String): Unit =
    val o1 = newType.getFacetObject(facetName1)
    val o2 = newType.getFacetObject(facetName2)

    if o1 != null && o2 != null then
      val c = o1.getConcreteType().asInstanceOf[Comparator].compare(
        o1.asInstanceOf[RangeFacet].limitValue,
        o2.asInstanceOf[RangeFacet].limitValue
      )
      if c == Comparator.GREATER then
        throw reportFacetInconsistency(newType.displayName(), o1, facetName1, o2, facetName2)

  private def reportFacetInconsistency(
    newName: String,
    o1: DataTypeWithFacet,
    facetName1: String,
    o2: DataTypeWithFacet,
    facetName2: String
  ): DatatypeException =
    val o1typeName = o1.getName
    val o2typeName = o2.getName

    if o1typeName == o2typeName then
      new DatatypeException(
        XSDatatypeImpl.localize(XSDatatypeImpl.ERR_INCONSISTENT_FACETS_1, facetName1, facetName2)
      )
    else if o1typeName == newName then
      new DatatypeException(
        XSDatatypeImpl.localize(
          XSDatatypeImpl.ERR_INCONSISTENT_FACETS_2,
          facetName1,
          o2.displayName(),
          facetName2
        )
      )
    else if o2typeName == newName then
      new DatatypeException(
        XSDatatypeImpl.localize(
          XSDatatypeImpl.ERR_INCONSISTENT_FACETS_2,
          facetName2,
          o1.displayName(),
          facetName1
        )
      )
    else
      throw new IllegalStateException()

  private def isValueFacet(facetName: String): Boolean =
    facetName == XSDatatype.FACET_ENUMERATION ||
      facetName == XSDatatype.FACET_MAXEXCLUSIVE ||
      facetName == XSDatatype.FACET_MINEXCLUSIVE ||
      facetName == XSDatatype.FACET_MAXINCLUSIVE ||
      facetName == XSDatatype.FACET_MININCLUSIVE

  private def isRepeatable(facetName: String): Boolean =
    facetName == XSDatatype.FACET_ENUMERATION || facetName == XSDatatype.FACET_PATTERN
