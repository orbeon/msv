/*
 * @(#)$Id: ITimeDurationValueType.java,v 1.10 2003/06/09 20:49:25 kk122374 Exp $
 *
 * Copyright 2001 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * This software is the proprietary information of Sun Microsystems, Inc.  
 * Use is subject to license terms.
 * 
 */
package org.orbeon.msv.datatype.xsd.datetime

/**
 * Interface as a value type of TimeDurationType
 * 
 * @author Kohsuke KAWAGUCHI
 */
trait ITimeDurationValueType extends java.io.Serializable {
  def getBigValue(): BigTimeDurationValueType

  /**
   * Compare two ITimeDurationValueType as defined in
   * org.orbeon.msv.datatype.xsd.Comparator
   */
  def compare(rhs: ITimeDurationValueType): Int
}
