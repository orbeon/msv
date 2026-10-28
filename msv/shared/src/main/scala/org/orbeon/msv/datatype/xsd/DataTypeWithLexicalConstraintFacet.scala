/*
 * @(#)$Id: DataTypeWithLexicalConstraintFacet.java,v 1.18 2003/06/09 20:49:21 kk122374 Exp $
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
 * Base class for facets which constrains lexical space of data.
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
abstract class DataTypeWithLexicalConstraintFacet(
  nsUri: String,
  typeName: String,
  baseType: XSDatatypeImpl,
  facetName: String,
  isFixed: Boolean
) extends DataTypeWithFacet(nsUri, typeName, baseType, facetName, isFixed):

  final def checkFormat(literal: String, context: ValidationContext): Boolean =
    if !baseType.checkFormat(literal, context) then false
    else checkLexicalConstraint(literal)

  final def _createValue(literal: String, context: ValidationContext): AnyRef =
    val o = baseType._createValue(literal, context)
    if o != null && !checkLexicalConstraint(literal) then null
    else o

  protected def checkLexicalConstraint(literal: String): Boolean
