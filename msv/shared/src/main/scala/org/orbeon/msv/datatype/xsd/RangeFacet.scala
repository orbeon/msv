/*
 * @(#)$Id: RangeFacet.java,v 1.23 2003/06/09 20:49:24 kk122374 Exp $
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
 * Base class of "(max|min)(In|Ex)clusive" facet validator.
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
abstract class RangeFacet(
  nsUri: String,
  typeName: String,
  baseType: XSDatatypeImpl,
  facetName: String,
  val limitValue: AnyRef,
  isFixed: Boolean
) extends DataTypeWithValueConstraintFacet(nsUri, typeName, baseType, facetName, isFixed):

  final def _createValue(literal: String, context: ValidationContext): AnyRef =
    val o = baseType._createValue(literal, context)
    if o == null then return null

    val r = concreteType.asInstanceOf[Comparator].compare(limitValue, o)
    if !rangeCheck(r) then null
    else o

  @throws(classOf[DatatypeException])
  protected def diagnoseByFacet(content: String, context: ValidationContext): Unit =
    if _createValue(content, context) != null then return

    throw new DatatypeException(
      DatatypeException.UNKNOWN,
      XSDatatypeImpl.localize(XSDatatypeImpl.ERR_OUT_OF_RANGE, facetName, limitValue)
    )

  protected def rangeCheck(compareResult: Int): Boolean
