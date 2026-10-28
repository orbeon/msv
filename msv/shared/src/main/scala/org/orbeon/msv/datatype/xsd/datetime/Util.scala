/*
 * @(#)$Id: Util.java,v 1.12 2005/03/31 23:19:07 kohsuke Exp $
 *
 * Copyright 2001 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * This software is the proprietary information of Sun Microsystems, Inc.  
 * Use is subject to license terms.
 * 
 */
package org.orbeon.msv.datatype.xsd.datetime

import org.orbeon.msv.datatype.xsd.Comparator
import java.math.{BigDecimal, BigInteger}

/**
 * Date/time-related utility functions/variables.
 * 
 * @author Kohsuke KAWAGUCHI
 */
private[datetime] object Util {

  // Frequently used constants
  val the4: BigInteger   = new BigInteger("4")
  val the10: BigInteger  = new BigInteger("10")
  val the12: BigInteger  = new BigInteger("12")
  val the24: BigInteger  = new BigInteger("24")
  val the60: BigInteger  = new BigInteger("60")
  val the100: BigInteger = new BigInteger("100")
  val the400: BigInteger = new BigInteger("400")
  /** Number of minutes in 400 years. */
  val the210379680: BigInteger = new BigInteger("210379680")

  val decimal0: BigDecimal = new BigDecimal(BigInteger.ZERO, 0)
  val int0: java.lang.Integer = java.lang.Integer.valueOf(0)

  val timeZonePos14: TimeZone = TimeZone(14 * 60)
  val timeZoneNeg14: TimeZone = TimeZone(-14 * 60)

  /**
   * Compare two objects
   * 
   * @return true
   *   - if both are null
   *   - if both are non-null and o1.equals(o2)
   * false otherwise.
   */
  def objEqual(o1: Any, o2: Any): Boolean =
    if (o1 == null && o2 == null) true
    else if (o1 != null && o2 != null && o1 == o2) true
    else false

  def objHashCode(o: Any): Int =
    if (o == null) 0
    else o.hashCode()

  /**
   * Compares two Comparable objects (possibly null) and returns
   * one of Comparator constant.
   */
  def objCompare[T](o1: java.lang.Comparable[T], o2: java.lang.Comparable[T]): Int = {
    if (o1 == null && o2 == null) return Comparator.EQUAL
    if (o1 != null && o2 != null) {
      val r = o1.asInstanceOf[java.lang.Comparable[Any]].compareTo(o2)
      if (r < 0) return Comparator.LESS
      if (r > 0) return Comparator.GREATER
      return Comparator.EQUAL
    }
    Comparator.UNDECIDABLE
  }

  /** Creates BigInteger that corresponds with v */
  def int2bi(v: Int): BigInteger =
    new BigInteger(Integer.toString(v))

  def int2bi(v: java.lang.Integer): BigInteger = {
    if (v == null) BigInteger.ZERO
    else new BigInteger(v.toString)
  }

  private val dayInMonth = Array(31, -1, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)

  def maximumDayInMonthFor(year: Int, month: Int): Int = {
    if (month == 1) {
      if (year % 400 == 0) return 29
      if (year % 4 == 0 && year % 100 != 0) return 29
      return 28
    }
    dayInMonth(month)
  }

  def maximumDayInMonthFor(year: BigInteger, month: Int): Int = {
    if (month == 1) {
      if (year.mod(Util.the400).intValue() == 0) return 29
      if (year.mod(Util.the4).intValue() == 0 && year.mod(Util.the100).intValue() != 0) return 29
      return 28
    }
    dayInMonth(month)
  }
}
