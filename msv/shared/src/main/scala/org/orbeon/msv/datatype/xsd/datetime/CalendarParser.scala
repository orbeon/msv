/*
 * @(#)$Id: CalendarParser.java,v 1.5 2004/05/07 16:01:44 kohsuke Exp $
 *
 * Copyright 2001 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * This software is the proprietary information of Sun Microsystems, Inc.  
 * Use is subject to license terms.
 * 
 */
package org.orbeon.msv.datatype.xsd.datetime

import java.util.{Calendar, GregorianCalendar}

/**
 * Parses XML Schema date/time related types into {@link java.util.Calendar}.
 * 
 * @author Kohsuke Kawaguchi (kohsuke.kawaguchi@sun.com)
 */
final class CalendarParser private (format: String, value: String) extends AbstractCalendarParser(format, value) {

  private val cal = new GregorianCalendar(0, 0, 0)
  cal.clear(Calendar.YEAR)
  cal.clear(Calendar.MONTH)
  cal.clear(Calendar.DAY_OF_MONTH)
  cal.setLenient(false)

  protected def parseFractionSeconds(): Unit = {
    cal.set(Calendar.MILLISECOND, parseInt(1, 3))
    skipDigits()
  }

  protected def setTimeZone(tz: java.util.TimeZone): Unit =
    cal.setTimeZone(tz)

  protected def setSeconds(i: Int): Unit =
    cal.set(Calendar.SECOND, i)

  protected def setMinutes(i: Int): Unit =
    cal.set(Calendar.MINUTE, i)

  protected def setHours(i: Int): Unit =
    cal.set(Calendar.HOUR_OF_DAY, i)

  protected def setDay(i: Int): Unit =
    cal.set(Calendar.DAY_OF_MONTH, i)

  protected def setMonth(i: Int): Unit =
    cal.set(Calendar.MONTH, i - 1)

  protected def setYear(i: Int): Unit =
    cal.set(Calendar.YEAR, i)
}

object CalendarParser {
  @throws(classOf[IllegalArgumentException])
  def parse(format: String, value: String): GregorianCalendar = {
    val parser = new CalendarParser(format, value)
    parser.parse()
    parser.cal.getTimeInMillis
    parser.cal
  }
}
