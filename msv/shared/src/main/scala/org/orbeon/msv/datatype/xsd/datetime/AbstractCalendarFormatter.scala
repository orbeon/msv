/*
 * @(#)$Id: AbstractCalendarFormatter.java,v 1.1 2003/12/16 21:39:13 kohsuke Exp $
 *
 * Copyright 2001 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * This software is the proprietary information of Sun Microsystems, Inc.  
 * Use is subject to license terms.
 * 
 */
package org.orbeon.msv.datatype.xsd.datetime

/**
 * @author Kohsuke Kawaguchi (kohsuke.kawaguchi@sun.com)
 */
abstract class AbstractCalendarFormatter {

  @throws(classOf[IllegalArgumentException])
  def doFormat(format: String, cal: AnyRef): String = {
    var fidx = 0
    val flen = format.length
    val buf = new java.lang.StringBuilder()

    while (fidx < flen) {
      val fch = format.charAt(fidx)
      fidx += 1

      if (fch != '%') {
        buf.append(fch)
      } else {
        val nextCh = format.charAt(fidx)
        fidx += 1
        nextCh match {
          case 'Y' => formatYear(cal, buf)
          case 'M' => formatMonth(cal, buf)
          case 'D' => formatDays(cal, buf)
          case 'h' => formatHours(cal, buf)
          case 'm' => formatMinutes(cal, buf)
          case 's' => formatSeconds(cal, buf)
          case 'z' => formatTimeZone(cal, buf)
          case _   => throw new InternalError()
        }
      }
    }

    buf.toString
  }

  protected def getTimeZone(cal: AnyRef): TimeZone
  protected def formatYear(cal: AnyRef, buf: java.lang.StringBuilder): Unit
  protected def formatMonth(cal: AnyRef, buf: java.lang.StringBuilder): Unit
  protected def formatDays(cal: AnyRef, buf: java.lang.StringBuilder): Unit
  protected def formatHours(cal: AnyRef, buf: java.lang.StringBuilder): Unit
  protected def formatMinutes(cal: AnyRef, buf: java.lang.StringBuilder): Unit
  protected def formatSeconds(cal: AnyRef, buf: java.lang.StringBuilder): Unit

  /** Formats time zone specifier. */
  private def formatTimeZone(cal: AnyRef, buf: java.lang.StringBuilder): Unit = {
    val tz = getTimeZone(cal)

    if (tz == null || tz == TimeZone.MISSING) return
    if (tz == TimeZone.ZERO) {
      buf.append('Z')
      return
    }

    val minutes = tz.minutes
    val offset =
      if (minutes >= 0) {
        buf.append('+')
        minutes
      } else {
        buf.append('-')
        -minutes
      }

    formatTwoDigits(offset / 60, buf)
    buf.append(':')
    formatTwoDigits(offset % 60, buf)
  }

  /** Formats Integer into two-character-wide string. */
  protected final def formatTwoDigits(n: Int, buf: java.lang.StringBuilder): Unit = {
    if (n < 10) buf.append('0')
    buf.append(n)
  }
}
