/*
 * @(#)$Id: MinLengthFacet.java,v 1.24 2003/06/09 20:49:23 kk122374 Exp $
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
 * 'minLength' facet.
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class MinLengthFacet(
  nsUri: String,
  typeName: String,
  baseType: XSDatatypeImpl,
  val minLength: Int,
  isFixed: Boolean
) extends DataTypeWithValueConstraintFacet(nsUri, typeName, baseType, XSDatatype.FACET_MINLENGTH, isFixed):

  def this(nsUri: String, typeName: String, baseType: XSDatatypeImpl, facets: TypeIncubator) =
    this(
      nsUri,
      typeName,
      baseType,
      facets.getNonNegativeInteger(XSDatatype.FACET_MINLENGTH),
      facets.isFixed(XSDatatype.FACET_MINLENGTH)
    )

  // loosened facet check
  val o: DataTypeWithFacet = baseType.getFacetObject(XSDatatype.FACET_MINLENGTH)
  if o != null && o.asInstanceOf[MinLengthFacet].minLength > this.minLength then
    throw new DatatypeException(
      XSDatatypeImpl.localize(XSDatatypeImpl.ERR_LOOSENED_FACET, XSDatatype.FACET_MINLENGTH, o.displayName())
    )

  def _createValue(literal: String, context: ValidationContext): AnyRef =
    val o = baseType._createValue(literal, context)
    if o == null || concreteType.asInstanceOf[Discrete].countLength(o) < minLength then null
    else o

  @throws(classOf[DatatypeException])
  protected def diagnoseByFacet(content: String, context: ValidationContext): Unit =
    val o = concreteType._createValue(content, context)
    if o == null then throw new IllegalStateException()

    val cnt = concreteType.asInstanceOf[Discrete].countLength(o)
    if cnt < minLength then
      throw new DatatypeException(
        DatatypeException.UNKNOWN,
        XSDatatypeImpl.localize(
          XSDatatypeImpl.ERR_MINLENGTH,
          java.lang.Integer.valueOf(cnt),
          java.lang.Integer.valueOf(minLength)
        )
      )
