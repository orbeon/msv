/*
 * @(#)$Id: NumberType.java,v 1.23 2005/03/31 23:19:10 kohsuke Exp $
 *
 * Copyright 2001 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * This software is the proprietary information of Sun Microsystems, Inc.  
 * Use is subject to license terms.
 * 
 */
package org.orbeon.msv.datatype.xsd

import org.orbeon.msv.datatype.SerializationContext
import org.orbeon.msv.relaxng.datatype.ValidationContext

import java.math.{BigDecimal, BigInteger}

/**
 * "decimal" type.
 * 
 * Type of the value object is `java.math.BigDecimal`.
 * See http://www.w3.org/TR/xmlschema-2/#decimal for the spec.
 * It was once known as "number" type.
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class NumberType private () extends BuiltinAtomicType("decimal") with Comparator:

  final def getBaseType(): XSDatatype = SimpleURType.theInstance

  def checkFormat(content: String, context: ValidationContext): Boolean =
    val len = content.length
    var i = 0
    var atLeastOneDigit = false

    if len == 0 then return false

    // leading optional sign
    var ch = content.charAt(0)
    if ch == '-' || ch == '+' then i += 1

    var broken = false
    while i < len && !broken do
      ch = content.charAt(i)
      i += 1
      if '0' <= ch && ch <= '9' then
        atLeastOneDigit = true
      else if ch == '.' then
        broken = true
      else
        return false

    while i < len do
      ch = content.charAt(i)
      i += 1
      if '0' <= ch && ch <= '9' then
        atLeastOneDigit = true
      else
        return false

    atLeastOneDigit

  def _createValue(content: String, context: ValidationContext): AnyRef =
    if !checkFormat(content, context) then null
    else NumberType.load(content)

  def getJavaObjectType(): Class[?] = classOf[BigDecimal]

  @throws(classOf[IllegalArgumentException])
  def convertToLexicalValue(o: AnyRef, context: SerializationContext): String =
    o match
      case bd: BigDecimal => bd.toString
      case _              => throw new IllegalArgumentException()

  final def isFacetApplicable(facetName: String): Int =
    if facetName == XSDatatype.FACET_TOTALDIGITS ||
       facetName == XSDatatype.FACET_FRACTIONDIGITS ||
       facetName == XSDatatype.FACET_PATTERN ||
       facetName == XSDatatype.FACET_ENUMERATION ||
       facetName == XSDatatype.FACET_WHITESPACE ||
       facetName == XSDatatype.FACET_MAXINCLUSIVE ||
       facetName == XSDatatype.FACET_MININCLUSIVE ||
       facetName == XSDatatype.FACET_MAXEXCLUSIVE ||
       facetName == XSDatatype.FACET_MINEXCLUSIVE then
      XSDatatype.APPLICABLE
    else
      XSDatatype.NOT_ALLOWED

  final def compare(o1: AnyRef, o2: AnyRef): Int =
    val r = o1.asInstanceOf[Comparable[AnyRef]].compareTo(o2)
    if r < 0 then Comparator.LESS
    else if r > 0 then Comparator.GREATER
    else Comparator.EQUAL

object NumberType:
  val theInstance: NumberType = new NumberType()

  private val the10 = new BigInteger("10")

  def load(content0: String): BigDecimal =
    try
      if content0.length == 0 then return null
      val content =
        if content0.charAt(0) == '+' then content0.substring(1)
        else content0

      var r = new BigDecimal(content)

      // normalize BigDecimal trailing zeros in fractional digits
      while r.scale() > 0 do
        val q_r = r.unscaledValue().divideAndRemainder(the10)
        if !q_r(1).equals(BigInteger.ZERO) then
          return r
        r = new BigDecimal(q_r(0), r.scale() - 1)

      r
    catch
      case _: NumberFormatException => null

  def save(o: AnyRef): String =
    o.asInstanceOf[BigDecimal].toString
