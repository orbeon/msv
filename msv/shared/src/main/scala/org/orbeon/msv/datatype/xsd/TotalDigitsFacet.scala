/*
 * @(#)$Id: TotalDigitsFacet.java,v 1.17 2003/06/09 20:49:24 kk122374 Exp $
 *
 * Copyright 2001 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * This software is the proprietary information of Sun Microsystems, Inc.  
 * Use is subject to license terms.
 * 
 */
package org.orbeon.msv.datatype.xsd

import org.orbeon.msv.relaxng.datatype.{DatatypeException, ValidationContext}

/**
 * 'totalDigits' facet.
 *
 * This class holds these facet information and performs validation.
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class TotalDigitsFacet(
  nsUri: String,
  typeName: String,
  baseType: XSDatatypeImpl,
  val precision: Int,
  isFixed: Boolean
) extends DataTypeWithLexicalConstraintFacet(nsUri, typeName, baseType, XSDatatype.FACET_TOTALDIGITS, isFixed):

  // loosened facet check
  val o: DataTypeWithFacet = baseType.getFacetObject(XSDatatype.FACET_TOTALDIGITS)
  if o != null && o.asInstanceOf[TotalDigitsFacet].precision < this.precision then
    throw new DatatypeException(
      XSDatatypeImpl.localize(XSDatatypeImpl.ERR_LOOSENED_FACET, XSDatatype.FACET_TOTALDIGITS, o.displayName())
    )

  protected def checkLexicalConstraint(content: String): Boolean =
    TotalDigitsFacet.countPrecision(content) <= precision

  @throws(classOf[DatatypeException])
  protected def diagnoseByFacet(content: String, context: ValidationContext): Unit =
    val cnt = TotalDigitsFacet.countPrecision(content)
    if cnt <= precision then return

    throw new DatatypeException(
      DatatypeException.UNKNOWN,
      XSDatatypeImpl.localize(
        XSDatatypeImpl.ERR_TOO_MUCH_PRECISION,
        java.lang.Integer.valueOf(cnt),
        java.lang.Integer.valueOf(precision)
      )
    )

object TotalDigitsFacet:
  /** Counts the number of digits */
  def countPrecision(literal: String): Int =
    val start =
      if literal.nonEmpty && (literal.charAt(0) == '+' || literal.charAt(0) == '-') then 1
      else 0

    val dot = literal.indexOf('.')
    val intEnd = if dot < 0 then literal.length else dot
    val fracStart = if dot < 0 then literal.length else dot + 1
    var fracEnd = literal.length

    // Integer trailing zeros are significant. Only trim leading zeros.
    var intStart = start
    while intStart < intEnd && literal.charAt(intStart) == '0' do
      intStart += 1
    val intDigits = intEnd - intStart

    // Fraction leading zeros are significant, but trailing zeros are not.
    while fracEnd > fracStart && literal.charAt(fracEnd - 1) == '0' do
      fracEnd -= 1
    val fracDigits = fracEnd - fracStart

    val total = intDigits + fracDigits
    // Zero still has precision 1.
    if total == 0 then 1 else total
