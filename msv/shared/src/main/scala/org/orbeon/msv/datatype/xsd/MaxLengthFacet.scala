/*
 * @(#)$Id: MaxLengthFacet.java,v 1.24 2003/06/09 20:49:23 kk122374 Exp $
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
 * 'maxLength' facet.
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class MaxLengthFacet(
  nsUri: String,
  typeName: String,
  baseType: XSDatatypeImpl,
  val maxLength: Int,
  isFixed: Boolean
) extends DataTypeWithValueConstraintFacet(nsUri, typeName, baseType, XSDatatype.FACET_MAXLENGTH, isFixed):

  def this(nsUri: String, typeName: String, baseType: XSDatatypeImpl, facets: TypeIncubator) =
    this(
      nsUri,
      typeName,
      baseType,
      facets.getNonNegativeInteger(XSDatatype.FACET_MAXLENGTH),
      facets.isFixed(XSDatatype.FACET_MAXLENGTH)
    )

  // loosened facet check
  val o: DataTypeWithFacet = baseType.getFacetObject(XSDatatype.FACET_MAXLENGTH)
  if o != null && o.asInstanceOf[MaxLengthFacet].maxLength < this.maxLength then
    throw new DatatypeException(
      XSDatatypeImpl.localize(XSDatatypeImpl.ERR_LOOSENED_FACET, XSDatatype.FACET_MAXLENGTH, o.displayName())
    )

  def _createValue(literal: String, context: ValidationContext): AnyRef =
    val o = baseType._createValue(literal, context)
    if o == null || concreteType.asInstanceOf[Discrete].countLength(o) > maxLength then null
    else o

  @throws(classOf[DatatypeException])
  protected def diagnoseByFacet(content: String, context: ValidationContext): Unit =
    val o = concreteType._createValue(content, context)
    if o == null then throw new IllegalStateException()

    val cnt = concreteType.asInstanceOf[Discrete].countLength(o)
    if cnt > maxLength then
      throw new DatatypeException(
        DatatypeException.UNKNOWN,
        XSDatatypeImpl.localize(
          XSDatatypeImpl.ERR_MAXLENGTH,
          java.lang.Integer.valueOf(cnt),
          java.lang.Integer.valueOf(maxLength)
        )
      )
