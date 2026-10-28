/*
 * @(#)$Id: DateTimeFactory.java,v 1.11 2003/12/16 21:39:13 kohsuke Exp $
 *
 * Copyright 2001 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * This software is the proprietary information of Sun Microsystems, Inc.  
 * Use is subject to license terms.
 * 
 */
package org.orbeon.msv.datatype.xsd.datetime

import java.math.{BigDecimal, BigInteger}

/**
 * Utility functions that creates date/time related objects.
 * 
 * @author Kohsuke KAWAGUCHI
 */
object DateTimeFactory {

  def createFromDateTime(
    yearIn: Number,
    month: java.lang.Integer,
    day: java.lang.Integer,
    hour: java.lang.Integer,
    minute: java.lang.Integer,
    mSecond: Number,
    zone: java.util.TimeZone
  ): IDateTimeValueType = {
    val year: BigInteger =
      if (yearIn == null) null
      else if (yearIn.isInstanceOf[BigInteger]) yearIn.asInstanceOf[BigInteger]
      else new BigInteger(yearIn.toString)

    val second: BigDecimal =
      if (mSecond != null) {
        if (mSecond.isInstanceOf[java.lang.Integer] || mSecond.isInstanceOf[Int])
          new BigDecimal(mSecond.toString).movePointLeft(3)
        else if (mSecond.isInstanceOf[BigDecimal])
          mSecond.asInstanceOf[BigDecimal].movePointLeft(3)
        else
          throw new UnsupportedOperationException()
      } else {
        null
      }

    new BigDateTimeValueType(year, month, day, hour, minute, second, zone)
  }

  def createFromDate(
    year: Number,
    month: java.lang.Integer,
    day: java.lang.Integer,
    zone: java.util.TimeZone
  ): IDateTimeValueType =
    createFromDateTime(year, month, day, null, null, null, zone)

  def createFromTime(
    hour: java.lang.Integer,
    minute: java.lang.Integer,
    mSecond: Number,
    zone: java.util.TimeZone
  ): IDateTimeValueType =
    createFromDateTime(null, null, null, hour, minute, mSecond, zone)
}
