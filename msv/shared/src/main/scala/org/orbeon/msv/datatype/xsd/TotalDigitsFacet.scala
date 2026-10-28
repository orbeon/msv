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
    val len = literal.length
    var skipMode = true
    var seenDot = false
    var count = 0
    var trailingZero = 0

    var i = 0
    while i < len do
      val ch = literal.charAt(i)
      if ch == '.' then
        skipMode = false // digits after '.' is considered significant.
        seenDot = true

      if skipMode then
        // in skip mode, leading zeros are skipped
        if '1' <= ch && ch <= '9' then
          count += 1
          skipMode = false
      else
        if seenDot && ch == '0' then trailingZero += 1
        else trailingZero = 0

        if '0' <= ch && ch <= '9' then
          count += 1
      i += 1

    count - trailingZero
