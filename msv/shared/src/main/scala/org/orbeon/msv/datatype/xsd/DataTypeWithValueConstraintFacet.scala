/*
 * @(#)$Id: DataTypeWithValueConstraintFacet.java,v 1.18 2003/06/09 20:49:21 kk122374 Exp $
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
 * Base class for facets which constrain value space.
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
abstract class DataTypeWithValueConstraintFacet(
  nsUri: String,
  typeName: String,
  baseType: XSDatatypeImpl,
  facetName: String,
  isFixed: Boolean
) extends DataTypeWithFacet(nsUri, typeName, baseType, facetName, isFixed):

  override final def needValueCheck(): Boolean = true

  final def checkFormat(literal: String, context: ValidationContext): Boolean =
    _createValue(literal, context) != null
