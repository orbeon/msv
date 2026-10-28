/*
 * @(#)$Id: PreciseCalendarFormatter.java,v 1.2 2004/02/11 22:55:31 kohsuke Exp $
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
 * @author Kohsuke Kawaguchi (kohsuke.kawaguchi@sun.com)
 */
class PreciseCalendarFormatter private () extends AbstractCalendarFormatter {

  protected def getTimeZone(cal: AnyRef): TimeZone =
    cal.asInstanceOf[IDateTimeValueType].getBigValue().getTimeZone

  protected def formatYear(cal: AnyRef, buf: java.lang.StringBuilder): Unit = {
    val bv = cal.asInstanceOf[IDateTimeValueType].getBigValue()
    val year = bv.getYear
    if (year == null) {
      buf.append("0000")
      return
    }

    var s =
      if (year.signum() <= 0) {
        buf.append('-')
        year.negate().add(BigInteger.ONE).toString
      } else {
        year.toString
      }

    while (s.length < 4) s = "0" + s
    buf.append(s)
  }

  protected def formatMonth(cal: AnyRef, buf: java.lang.StringBuilder): Unit = {
    val bv = cal.asInstanceOf[IDateTimeValueType].getBigValue()
    formatTwoDigits(bv.getMonth, 1, buf)
  }

  protected def formatDays(cal: AnyRef, buf: java.lang.StringBuilder): Unit = {
    val bv = cal.asInstanceOf[IDateTimeValueType].getBigValue()
    formatTwoDigits(bv.getDay, 1, buf)
  }

  protected def formatHours(cal: AnyRef, buf: java.lang.StringBuilder): Unit = {
    val bv = cal.asInstanceOf[IDateTimeValueType].getBigValue()
    formatTwoDigits(bv.getHour, buf)
  }

  protected def formatMinutes(cal: AnyRef, buf: java.lang.StringBuilder): Unit = {
    val bv = cal.asInstanceOf[IDateTimeValueType].getBigValue()
    formatTwoDigits(bv.getMinute, buf)
  }

  protected def formatSeconds(cal: AnyRef, buf: java.lang.StringBuilder): Unit = {
    val bv = cal.asInstanceOf[IDateTimeValueType].getBigValue()
    var sec = bv.getSecond

    if (sec == null) {
      buf.append("00")
      return
    }

    while (sec.scale() > 0 && sec.toString.endsWith("0")) {
      sec = sec.movePointLeft(1)
    }

    var s = sec.toString
    if (sec.compareTo(new BigDecimal("10")) < 0) {
      s = "0" + s
    }
    buf.append(s)
  }

  private def formatTwoDigits(v: java.lang.Integer, buf: java.lang.StringBuilder): Unit =
    formatTwoDigits(v, 0, buf)

  private def formatTwoDigits(v: java.lang.Integer, offset: Int, buf: java.lang.StringBuilder): Unit = {
    if (v == null) buf.append("00")
    else formatTwoDigits(v.intValue() + offset, buf)
  }
}

object PreciseCalendarFormatter {
  private val theInstance = new PreciseCalendarFormatter()

  def format(format: String, cal: IDateTimeValueType): String =
    theInstance.doFormat(format, cal.getBigValue())
}
