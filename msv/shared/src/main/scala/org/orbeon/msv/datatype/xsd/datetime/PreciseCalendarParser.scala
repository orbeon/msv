/*
 * @(#)$Id: PreciseCalendarParser.java,v 1.1 2003/12/16 21:39:13 kohsuke Exp $
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
class PreciseCalendarParser private (format: String, value: String) extends AbstractCalendarParser(format, value) {

  private var year: BigInteger = null
  private var month: java.lang.Integer = null
  private var day: java.lang.Integer = null
  private var hour: java.lang.Integer = null
  private var minute: java.lang.Integer = null
  private var second: BigDecimal = null
  private var timeZone: TimeZone = null

  private def createCalendar(): BigDateTimeValueType =
    new BigDateTimeValueType(year, month, day, hour, minute, second, timeZone)

  protected def parseFractionSeconds(): Unit = {
    val s = vidx
    val bi = parseBigInteger(1, Int.MaxValue)
    val d = new BigDecimal(bi, vidx - s)
    if (second == null) second = d
    else second = second.add(d)
  }

  protected def setTimeZone(tz: TimeZone): Unit = {
    this.timeZone = if (tz == TimeZone.MISSING) null else tz
  }

  protected def setSeconds(i: Int): Unit = {
    val d = new BigDecimal(BigInteger.valueOf(i.toLong))
    if (second == null) second = d
    else second = second.add(d)
  }

  protected def setMinutes(i: Int): Unit = {
    minute = java.lang.Integer.valueOf(i)
  }

  protected def setHours(i: Int): Unit = {
    hour = java.lang.Integer.valueOf(i)
  }

  protected def setDay(i: Int): Unit = {
    day = java.lang.Integer.valueOf(i - 1)
  }

  protected def setMonth(i: Int): Unit = {
    month = java.lang.Integer.valueOf(i - 1)
  }

  protected def setYear(i: BigInteger): Unit = {
    // XSD has no year 0: -0001 is 1 BCE, -0002 is 2 BCE, etc.
    // Internally, value 0 means year -1, value -1 means year -2, etc.
    // So for non-positive XSD years, add 1 to convert to internal convention.
    year = if (i.signum() <= 0) i.add(BigInteger.ONE) else i
  }
}

object PreciseCalendarParser {
  @throws(classOf[IllegalArgumentException])
  def parse(format: String, value: String): BigDateTimeValueType = {
    val parser = new PreciseCalendarParser(format, value)
    parser.parse()
    parser.createCalendar()
  }
}
