/*
 * @(#)$Id: LengthFacet.java,v 1.25 2003/06/09 20:49:22 kk122374 Exp $
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
 * "length", "minLength", and "maxLength" facet validator.
 * 
 * This class also detects inconsistent facet setting
 * (for example, minLength=100 and maxLength=0).
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class LengthFacet(
  nsUri: String,
  typeName: String,
  baseType: XSDatatypeImpl,
  val length: Int,
  isFixed: Boolean
) extends DataTypeWithValueConstraintFacet(nsUri, typeName, baseType, XSDatatype.FACET_LENGTH, isFixed):

  def this(nsUri: String, typeName: String, baseType: XSDatatypeImpl, facets: TypeIncubator) =
    this(
      nsUri,
      typeName,
      baseType,
      facets.getNonNegativeInteger(XSDatatype.FACET_LENGTH),
      facets.isFixed(XSDatatype.FACET_LENGTH)
    )

  // loosened facet check
  val o: DataTypeWithFacet = baseType.getFacetObject(XSDatatype.FACET_LENGTH)
  if o != null && o.asInstanceOf[LengthFacet].length != this.length then
    throw new DatatypeException(
      XSDatatypeImpl.localize(XSDatatypeImpl.ERR_LOOSENED_FACET, XSDatatype.FACET_LENGTH, o.displayName())
    )

  def _createValue(content: String, context: ValidationContext): AnyRef =
    val o = baseType._createValue(content, context)
    if o == null || concreteType.asInstanceOf[Discrete].countLength(o) != length then null
    else o

  @throws(classOf[DatatypeException])
  protected def diagnoseByFacet(content: String, context: ValidationContext): Unit =
    val o = concreteType._createValue(content, context)
    if o == null then throw new IllegalStateException()

    val cnt = concreteType.asInstanceOf[Discrete].countLength(o)
    if cnt != length then
      throw new DatatypeException(
        DatatypeException.UNKNOWN,
        XSDatatypeImpl.localize(
          XSDatatypeImpl.ERR_LENGTH,
          java.lang.Integer.valueOf(cnt),
          java.lang.Integer.valueOf(length)
        )
      )
