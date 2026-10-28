/*
 * @(#)$Id: CalendarFormatter.java,v 1.1 2003/12/16 21:39:13 kohsuke Exp $
 *
 * Copyright 2001 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * This software is the proprietary information of Sun Microsystems, Inc.  
 * Use is subject to license terms.
 * 
 */
package org.orbeon.msv.datatype.xsd.datetime

import java.util.Calendar

/**
 * Formats a {@link Calendar} object to a String.
 * 
 * @author Kohsuke Kawaguchi (kohsuke.kawaguchi@sun.com)
 */
final class CalendarFormatter private () extends AbstractCalendarFormatter {

  protected def toCalendar(cal: AnyRef): Calendar =
    cal.asInstanceOf[Calendar]

  protected def formatYear(cal: AnyRef, buf: java.lang.StringBuilder): Unit = {
    val year = cal.asInstanceOf[Calendar].get(Calendar.YEAR)
    var s =
      if (year <= 0) Integer.toString(1 - year)
      else Integer.toString(year)

    while (s.length < 4) s = "0" + s
    if (year <= 0) s = "-" + s
    buf.append(s)
  }

  protected def formatMonth(cal: AnyRef, buf: java.lang.StringBuilder): Unit =
    formatTwoDigits(cal.asInstanceOf[Calendar].get(Calendar.MONTH) + 1, buf)

  protected def formatDays(cal: AnyRef, buf: java.lang.StringBuilder): Unit =
    formatTwoDigits(cal.asInstanceOf[Calendar].get(Calendar.DAY_OF_MONTH), buf)

  protected def formatHours(cal: AnyRef, buf: java.lang.StringBuilder): Unit =
    formatTwoDigits(cal.asInstanceOf[Calendar].get(Calendar.HOUR_OF_DAY), buf)

  protected def formatMinutes(cal: AnyRef, buf: java.lang.StringBuilder): Unit =
    formatTwoDigits(cal.asInstanceOf[Calendar].get(Calendar.MINUTE), buf)

  protected def formatSeconds(_cal: AnyRef, buf: java.lang.StringBuilder): Unit = {
    val cal = _cal.asInstanceOf[Calendar]
    formatTwoDigits(cal.get(Calendar.SECOND), buf)
    if (cal.isSet(Calendar.MILLISECOND)) {
      val n = cal.get(Calendar.MILLISECOND)
      if (n != 0) {
        var ms = Integer.toString(n)
        while (ms.length < 3) ms = "0" + ms
        buf.append('.')
        buf.append(ms)
      }
    }
  }
}

object CalendarFormatter {
  private val theInstance = new CalendarFormatter()

  def format(format: String, cal: Calendar): String =
    theInstance.doFormat(format, cal)
}
