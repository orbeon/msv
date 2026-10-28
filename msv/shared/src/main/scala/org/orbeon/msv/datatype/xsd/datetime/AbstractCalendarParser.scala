/*
 * @(#)$Id: AbstractCalendarParser.java,v 1.2 2005/03/31 23:18:13 kohsuke Exp $
 *
 * Copyright 2001 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * This software is the proprietary information of Sun Microsystems, Inc.  
 * Use is subject to license terms.
 * 
 */
package org.orbeon.msv.datatype.xsd.datetime

import java.math.BigInteger

/**
 * Parses XML Schema date/time related types into a structure.
 * 
 * @author Kohsuke Kawaguchi (kohsuke.kawaguchi@sun.com)
 */
abstract class AbstractCalendarParser(
  private val format: String,
  private val value: String
) {
  private val flen = format.length
  private val vlen = value.length

  private var fidx = 0
  protected var vidx = 0

  // track parsed date components for day-in-month validation
  private var parsedYear = Int.MinValue
  private var parsedMonth = Int.MinValue
  private var parsedDay = Int.MinValue

  @throws(classOf[IllegalArgumentException])
  def parse(): Unit = {
    while (fidx < flen) {
      val fch = format.charAt(fidx)
      fidx += 1

      if (fch != '%') {
        skip(fch)
      } else {
        val nextCh = format.charAt(fidx)
        fidx += 1
        nextCh match {
          case 'Y' =>
            var sign = 1
            if (peek() == '-') {
              vidx += 1
              sign = -1
            }
            val yearStart = vidx
            var yearBig = parseBigInteger(4, Int.MaxValue)
            if sign == -1 then
              yearBig = yearBig.negate()
            if yearBig.signum() == 0 then
              throw new IllegalArgumentException(value) // no year 0 in XSD
            val yearDigits = vidx - yearStart
            if yearDigits > 4 && value.charAt(yearStart) == '0' then
              throw new IllegalArgumentException(value) // no leading zeros on 5+ digit years
            try {
              parsedYear = yearBig.intValueExact()
            } catch {
              case _: ArithmeticException => /* too large for int */
            }
            setYear(yearBig)

          case 'M' =>
            val month = parseInt(2, 2)
            if month < 1 || month > 12 then
              throw new IllegalArgumentException(value)
            parsedMonth = month
            setMonth(month)

          case 'D' =>
            val day = parseInt(2, 2)
            if day < 1 || day > 31 then
              throw new IllegalArgumentException(value)
            parsedDay = day
            setDay(day)

          case 'h' =>
            setHours(parseInt(2, 2))

          case 'm' =>
            setMinutes(parseInt(2, 2))

          case 's' =>
            setSeconds(parseInt(2, 2))
            if (peek() == '.') {
              vidx += 1
              parseFractionSeconds()
            }

          case 'z' =>
            val vch = peek()
            if (vch == 'Z') {
              vidx += 1
              setTimeZone(TimeZone.ZERO)
            } else if (vch == '+' || vch == '-') {
              vidx += 1
              val h = parseInt(2, 2)
              skip(':')
              val m = parseInt(2, 2)
              val totalMinutes = h * 60 + m
              if totalMinutes > 14 * 60 then
                throw new IllegalArgumentException(value) // timezone offset out of range
              val offsetMinutes = (if (vch == '+') 1 else -1) * totalMinutes
              setTimeZone(TimeZone(offsetMinutes))
            } else {
              setTimeZone(TimeZone.MISSING)
            }

          case _ =>
            throw new InternalError()
        }
      }
    }

    if (vidx != vlen) {
      throw new IllegalArgumentException(value)
    }

    // validate day against month/year (day-in-month check)
    if (parsedDay != Int.MinValue && parsedMonth != Int.MinValue) {
      val y = if (parsedYear != Int.MinValue) parsedYear else 4 // default to leap year if year not specified
      val maxDay = Util.maximumDayInMonthFor(y, parsedMonth - 1) // Util uses 0-based month
      if (parsedDay > maxDay) {
        throw new IllegalArgumentException(value)
      }
    }
  }

  private def peek(): Char = {
    if (vidx == vlen) (-1).toChar
    else value.charAt(vidx)
  }

  private def read(): Char = {
    if (vidx == vlen) throw new IllegalArgumentException(value)
    val c = value.charAt(vidx)
    vidx += 1
    c
  }

  private def skip(ch: Char): Unit = {
    if (read() != ch) throw new IllegalArgumentException(value)
  }

  /** Skips the extra digits. */
  protected final def skipDigits(): Unit = {
    while (AbstractCalendarParser.isDigit(peek())) vidx += 1
  }

  @throws(classOf[IllegalArgumentException])
  protected final def parseInt(minDigits: Int, maxDigits: Int): Int = {
    val vstart = vidx
    while (AbstractCalendarParser.isDigit(peek()) && (vidx - vstart) < maxDigits) {
      vidx += 1
    }
    if ((vidx - vstart) < minDigits) {
      throw new IllegalArgumentException(value)
    }
    Integer.parseInt(value.substring(vstart, vidx))
  }

  @throws(classOf[IllegalArgumentException])
  protected final def parseBigInteger(minDigits: Int, maxDigits: Int): BigInteger = {
    val vstart = vidx
    while (AbstractCalendarParser.isDigit(peek()) && (vidx - vstart) <= maxDigits) {
      vidx += 1
    }
    if ((vidx - vstart) < minDigits) {
      throw new IllegalArgumentException(value)
    }
    new BigInteger(value.substring(vstart, vidx))
  }

  protected def parseFractionSeconds(): Unit
  protected def setTimeZone(tz: TimeZone): Unit
  protected def setSeconds(i: Int): Unit
  protected def setMinutes(i: Int): Unit
  protected def setHours(i: Int): Unit
  protected def setDay(i: Int): Unit
  protected def setMonth(i: Int): Unit
  protected def setYear(i: BigInteger): Unit
}

object AbstractCalendarParser {
  private def isDigit(ch: Char): Boolean =
    '0' <= ch && ch <= '9'
}
