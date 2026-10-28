/*
 * @(#)$Id: MinInclusiveFacet.java,v 1.14 2003/06/09 20:49:23 kk122374 Exp $
 *
 * Copyright 2001 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * This software is the proprietary information of Sun Microsystems, Inc.  
 * Use is subject to license terms.
 * 
 */
package org.orbeon.msv.datatype.xsd

/**
 * 'minInclusive' facet.
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class MinInclusiveFacet(
  nsUri: String,
  typeName: String,
  baseType: XSDatatypeImpl,
  limit: AnyRef,
  isFixed: Boolean
) extends RangeFacet(nsUri, typeName, baseType, XSDatatype.FACET_MININCLUSIVE, limit, isFixed):

  protected final def rangeCheck(r: Int): Boolean =
    r == Comparator.LESS || r == Comparator.EQUAL
