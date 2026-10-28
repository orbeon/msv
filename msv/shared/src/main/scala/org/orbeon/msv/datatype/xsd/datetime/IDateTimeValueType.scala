/*
 * @(#)$Id: IDateTimeValueType.java,v 1.10 2003/06/09 20:49:25 kk122374 Exp $
 *
 * Copyright 2001 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * This software is the proprietary information of Sun Microsystems, Inc.  
 * Use is subject to license terms.
 * 
 */
package org.orbeon.msv.datatype.xsd.datetime

/**
 * Interface as value type of DateTimeType
 * 
 * @author Kohsuke KAWAGUCHI
 */
trait IDateTimeValueType extends java.io.Serializable {
  def getBigValue(): BigDateTimeValueType

  /**
   * Returns the result of addition of this object and TimeDuration.
   * 
   * This object will not be mutated.
   */
  def add(duration: ITimeDurationValueType): IDateTimeValueType

  /**
   * Gets the normalized IDateTimeValueType.
   * 
   * Normalized value always has GMT timezone or no timezone
   */
  def normalize(): IDateTimeValueType

  /**
   * Compare two DateTimeValueType as defined in
   * org.orbeon.msv.datatype.xsd.Comparator
   */
  def compare(rhs: IDateTimeValueType): Int
}
