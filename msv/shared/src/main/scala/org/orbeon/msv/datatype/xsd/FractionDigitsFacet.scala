/*
 * @(#)$Id: FractionDigitsFacet.java,v 1.16 2003/06/09 20:49:21 kk122374 Exp $
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
 * 'fractionDigits' facet.
 *
 * This class holds these facet information and performs validation.
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class FractionDigitsFacet(
  nsUri: String,
  typeName: String,
  baseType: XSDatatypeImpl,
  val scale: Int,
  isFixed: Boolean
) extends DataTypeWithLexicalConstraintFacet(nsUri, typeName, baseType, XSDatatype.FACET_FRACTIONDIGITS, isFixed):

  // loosened facet check
  val o: DataTypeWithFacet = baseType.getFacetObject(XSDatatype.FACET_FRACTIONDIGITS)
  if o != null && o.asInstanceOf[FractionDigitsFacet].scale < this.scale then
    throw new DatatypeException(
      XSDatatypeImpl.localize(XSDatatypeImpl.ERR_LOOSENED_FACET, XSDatatype.FACET_FRACTIONDIGITS, o.displayName())
    )

  protected def checkLexicalConstraint(content: String): Boolean =
    FractionDigitsFacet.countScale(content) <= scale

  @throws(classOf[DatatypeException])
  protected def diagnoseByFacet(content: String, context: ValidationContext): Unit =
    val cnt = FractionDigitsFacet.countScale(content)
    if cnt <= scale then return

    throw new DatatypeException(
      DatatypeException.UNKNOWN,
      XSDatatypeImpl.localize(
        XSDatatypeImpl.ERR_TOO_MUCH_SCALE,
        java.lang.Integer.valueOf(cnt),
        java.lang.Integer.valueOf(scale)
      )
    )

object FractionDigitsFacet:
  /**
   * Count the number of fractional digits.
   * 
   * This method can assume that the given literal is appropriate
   * as a decimal value.
   */
  def countScale(literal: String): Int =
    val len = literal.length
    var skipMode = true
    var count = 0
    var trailingZero = 0

    var i = 0
    while i < len do
      val ch = literal.charAt(i)
      if skipMode then
        if ch == '.' then skipMode = false
      else
        if ch == '0' then trailingZero += 1
        else trailingZero = 0

        if '0' <= ch && ch <= '9' then
          count += 1
      i += 1

    count - trailingZero
