/*
 * @(#)$Id: FinalComponent.java,v 1.23 2003/06/09 20:49:21 kk122374 Exp $
 *
 * Copyright 2001 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * This software is the proprietary information of Sun Microsystems, Inc.  
 * Use is subject to license terms.
 * 
 */
package org.orbeon.msv.datatype.xsd

/**
 * "final" component.
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
final class FinalComponent(
  nsUri: String,
  newTypeName: String,
  baseType: XSDatatypeImpl,
  private val finalValue: Int
) extends Proxy(nsUri, newTypeName, baseType):

  def this(baseType: XSDatatypeImpl, finalValue: Int) =
    this(baseType.getNamespaceUri, baseType.getName, baseType, finalValue)

  override def isFinal(derivationType: Int): Boolean =
    if (finalValue & derivationType) != 0 then true
    else super.isFinal(derivationType)
