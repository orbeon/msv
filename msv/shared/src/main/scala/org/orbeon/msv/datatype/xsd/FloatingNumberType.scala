/*
 * @(#)$Id: FloatingNumberType.java,v 1.16 2003/06/09 20:49:21 kk122374 Exp $
 *
 * Copyright 2001 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * This software is the proprietary information of Sun Microsystems, Inc.  
 * Use is subject to license terms.
 * 
 */
package org.orbeon.msv.datatype.xsd

import org.orbeon.msv.relaxng.datatype.ValidationContext

/**
 * Base class of FloatType and DoubleType.
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
abstract class FloatingNumberType(typeName: String)
  extends BuiltinAtomicType(typeName) with Comparator:

  final def checkFormat(lexicalValue: String, context: ValidationContext): Boolean =
    _createValue(lexicalValue, context) != null

  final def compare(lhs: AnyRef, rhs: AnyRef): Int =
    val r = lhs.asInstanceOf[Comparable[AnyRef]].compareTo(rhs)
    if r < 0 then -1
    else if r > 0 then 1
    else 0

  final def isFacetApplicable(facetName: String): Int =
    if facetName == XSDatatype.FACET_PATTERN ||
       facetName == XSDatatype.FACET_ENUMERATION ||
       facetName == XSDatatype.FACET_WHITESPACE ||
       facetName == XSDatatype.FACET_MAXINCLUSIVE ||
       facetName == XSDatatype.FACET_MININCLUSIVE ||
       facetName == XSDatatype.FACET_MAXEXCLUSIVE ||
       facetName == XSDatatype.FACET_MINEXCLUSIVE then
      XSDatatype.APPLICABLE
    else
      XSDatatype.NOT_ALLOWED

object FloatingNumberType:
  def isDigitOrPeriodOrSign(ch: Char): Boolean =
    ('0' <= ch && ch <= '9') || ch == '+' || ch == '-' || ch == '.'

  def hasHexIndicator(s: String): Boolean =
    s.indexOf('x') >= 0 || s.indexOf('X') >= 0 || s.indexOf('p') >= 0 || s.indexOf('P') >= 0
