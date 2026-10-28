/*
 * @(#)$Id: IntegerDerivedType.java,v 1.28 2005/03/31 23:19:10 kohsuke Exp $
 *
 * Copyright 2001 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * This software is the proprietary information of Sun Microsystems, Inc.  
 * Use is subject to license terms.
 * 
 */
package org.orbeon.msv.datatype.xsd

import org.orbeon.msv.datatype.SerializationContext
import org.orbeon.msv.relaxng.datatype.{DatatypeException, ValidationContext}

/**
 * Base class for types derived from integer.
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
abstract class IntegerDerivedType(
  typeName: String,
  private val baseFacets: XSDatatypeImpl
) extends BuiltinAtomicType(typeName) with Comparator:

  final def isFacetApplicable(facetName: String): Int =
    if facetName == XSDatatype.FACET_TOTALDIGITS ||
       facetName == XSDatatype.FACET_PATTERN ||
       facetName == XSDatatype.FACET_WHITESPACE ||
       facetName == XSDatatype.FACET_ENUMERATION ||
       facetName == XSDatatype.FACET_MAXINCLUSIVE ||
       facetName == XSDatatype.FACET_MININCLUSIVE ||
       facetName == XSDatatype.FACET_MAXEXCLUSIVE ||
       facetName == XSDatatype.FACET_MINEXCLUSIVE then
      XSDatatype.APPLICABLE
    else if facetName == XSDatatype.FACET_FRACTIONDIGITS then
      XSDatatype.FIXED
    else
      XSDatatype.NOT_ALLOWED

  override def getFacetObject(facetName: String): DataTypeWithFacet =
    baseFacets.getFacetObject(facetName)

  final def checkFormat(content: String, context: ValidationContext): Boolean =
    _createValue(content, context) != null

  def convertToLexicalValue(value: AnyRef, context: SerializationContext): String =
    value match
      case _: Number | _: IntegerValueType => value.toString
      case _ => throw new IllegalArgumentException("invalid value type: " + value.getClass.toString)

  final def compare(o1: AnyRef, o2: AnyRef): Int =
    val r = o1.asInstanceOf[Comparable[AnyRef]].compareTo(o2)
    if r < 0 then Comparator.LESS
    else if r > 0 then Comparator.GREATER
    else Comparator.EQUAL

object IntegerDerivedType:

  /**
   * Removes leading optional '+' sign.
   */
  def removeOptionalPlus(s: String): String =
    if s.length <= 1 || s.charAt(0) != '+' then return s

    val sub = s.substring(1)
    val ch = sub.charAt(0)
    if ('0' <= ch && ch <= '9') || ch == '.' then sub
    else throw new NumberFormatException()

  /** Apply a range facet. */
  def createRangeFacet(baseType: XSDatatypeImpl, min: AnyRef, max: AnyRef): XSDatatypeImpl =
    try
      var r = baseType
      if min != null then
        r = new MinInclusiveFacet(null, null, r, min, false)
      if max != null then
        r = new MaxInclusiveFacet(null, null, r, max, false)
      r
    catch
      case _: DatatypeException =>
        throw new InternalError()
