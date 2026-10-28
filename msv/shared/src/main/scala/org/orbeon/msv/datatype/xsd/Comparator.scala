/*
 * @(#)$Id: Comparator.java,v 1.12 2003/06/09 20:49:20 kk122374 Exp $
 *
 * Copyright 2001 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * This software is the proprietary information of Sun Microsystems, Inc.  
 * Use is subject to license terms.
 * 
 */
package org.orbeon.msv.datatype.xsd

/**
 * Comparable datatype.
 *
 * Those datatypes which has order relation must implement this interface.
 * RangeFacet uses this interface to do its job.
 * It differs from {@link java.util.Comparator} in return value.
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
trait Comparator {
  /**
   * Compare two value types and decide their order relation
   */
  def compare(o1: AnyRef, o2: AnyRef): Int
}

object Comparator {
  val LESS: Int        = -1  // lhs < rhs
  val EQUAL: Int       = 0   // lhs = rhs
  val GREATER: Int     = 1   // lhs > rhs
  val UNDECIDABLE: Int = 999 // lhs ? rhs
}
