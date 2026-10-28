/*
 * @(#)$Id: WhiteSpaceFacet.java,v 1.21 2003/06/09 20:49:25 kk122374 Exp $
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
 * whiteSpace facet validator.
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class WhiteSpaceFacet(
  nsUri: String,
  typeName: String,
  baseType: XSDatatypeImpl,
  proc: WhiteSpaceProcessor,
  isFixed: Boolean
) extends DataTypeWithFacet(nsUri, typeName, baseType, XSDatatype.FACET_WHITESPACE, isFixed, proc):

  def this(nsUri: String, typeName: String, baseType: XSDatatypeImpl, facets: TypeIncubator) =
    this(
      nsUri,
      typeName,
      baseType,
      WhiteSpaceProcessor.get(facets.getFacet(XSDatatype.FACET_WHITESPACE).asInstanceOf[String]),
      facets.isFixed(XSDatatype.FACET_WHITESPACE)
    )

  // loosened facet check
  if baseType.whiteSpace.tightness() > this.whiteSpace.tightness() then
    var d: XSDatatype = baseType.getFacetObject(XSDatatype.FACET_WHITESPACE)
    if d == null then d = getConcreteType()

    throw new DatatypeException(
      XSDatatypeImpl.localize(XSDatatypeImpl.ERR_LOOSENED_FACET, XSDatatype.FACET_WHITESPACE, d.displayName())
    )

  def checkFormat(content: String, context: ValidationContext): Boolean =
    baseType.checkFormat(content, context)

  def _createValue(content: String, context: ValidationContext): AnyRef =
    baseType._createValue(content, context)

  /** whiteSpace facet never constrains anything */
  protected def diagnoseByFacet(content: String, context: ValidationContext): Unit = ()
