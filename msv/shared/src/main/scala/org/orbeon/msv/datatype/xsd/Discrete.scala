/*
 * @(#)$Id: Discrete.java,v 1.8 2003/06/09 20:49:21 kk122374 Exp $
 *
 * Copyright 2001 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * This software is the proprietary information of Sun Microsystems, Inc.  
 * Use is subject to license terms.
 * 
 */
package org.orbeon.msv.datatype.xsd

/**
 * Base interface of types to which length-related facets are applicable.
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
trait Discrete:
  /** Count the number of item in value type.
   * 
   * Actual semantics of this method varies.
   */
  def countLength(value: AnyRef): Int
