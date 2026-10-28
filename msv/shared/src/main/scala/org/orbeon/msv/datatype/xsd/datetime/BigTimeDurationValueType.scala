/*
 * @(#)$Id: BigTimeDurationValueType.java,v 1.16 2005/03/31 23:19:07 kohsuke Exp $
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
 * ITimeDurationValueType implementation that can hold all lexically legal
 * timeDuration value.
 * 
 * @author Kohsuke KAWAGUCHI
 */
class BigTimeDurationValueType extends ITimeDurationValueType {

  var signum: Int = 0

  // all the fields should be positive
  var year: BigInteger = BigInteger.ZERO
  var month: BigInteger = BigInteger.ZERO
  var day: BigInteger = BigInteger.ZERO
  var hour: BigInteger = BigInteger.ZERO
  var minute: BigInteger = BigInteger.ZERO
  var second: BigDecimal = Util.decimal0

  override def equals(o: Any): Boolean =
    o match {
      case oTyped: ITimeDurationValueType => compare(oTyped) == Comparator.EQUAL
      case _ => false
    }

  override def toString: String =
    (if (signum < 0) "-" else "") +
      "P" + nullAsZero(year).abs() + "Y" +
      nullAsZero(month) + "M" +
      nullAsZero(day) + "DT" +
      nullAsZero(hour) + "H" +
      nullAsZero(minute) + "M" +
      nullAsZero(second) + "S"

  private def nullAsZero(bi: BigInteger): BigInteger =
    if (bi == null) BigInteger.ZERO else bi

  private def nullAsZero(bd: BigDecimal): BigDecimal =
    if (bd == null) Util.decimal0 else bd

  override def hashCode(): Int = {
    // calculate equivalent seconds.
    // this hash code is not compliant with equals method.
    // for one thing, it's not possible to compute hash code which is
    // compliant with equals, since this order relation is partial order.
    val s =
      nullAsZero(year).multiply(BigInteger.valueOf(365 * 24 * 60 * 60))
        .add(nullAsZero(month).multiply(BigInteger.valueOf(30 * 24 * 60 * 60)))
        .add(nullAsZero(day).multiply(BigInteger.valueOf(24 * 60 * 60)))
        .add(nullAsZero(hour).multiply(BigInteger.valueOf(60 * 60)))
        .add(nullAsZero(minute).multiply(BigInteger.valueOf(60)))
        .add(nullAsZero(second).toBigInteger)

    if (signum < 0) s.negate().hashCode()
    else s.hashCode()
  }

  def compare(rhs: ITimeDurationValueType): Int =
    BigTimeDurationValueType.compare(this, rhs)

  def getBigValue(): BigTimeDurationValueType = this

  /**
   * All the fields should be positive and use the signum field to
   * determine the sign.
   */
  def this(
    signum: Int,
    year: BigInteger,
    month: BigInteger,
    day: BigInteger,
    hour: BigInteger,
    minute: BigInteger,
    second: BigDecimal
  ) = {
    this()
    this.signum = signum
    this.year   = if (year != null) year else BigInteger.ZERO
    this.month  = if (month != null) month else BigInteger.ZERO
    this.day    = if (day != null) day else BigInteger.ZERO
    this.hour   = if (hour != null) hour else BigInteger.ZERO
    this.minute = if (minute != null) minute else BigInteger.ZERO
    this.second = if (second != null) second else Util.decimal0
  }

  def getDay: BigInteger    = day
  def getHour: BigInteger   = hour
  def getMinute: BigInteger = minute
  def getMonth: BigInteger  = month
  def getSecond: BigDecimal = second
  def getYear: BigInteger   = year

  /**
   * Reads in the lexical duration format.
   * 
   * @param lexicalRepresentation whitespace stripped lexical form.
   */
  def this(lexicalRepresentation: String) = {
    this()
    val s = lexicalRepresentation
    val idx = Array(0)

    val positive =
      if (s.charAt(idx(0)) == '-') {
        idx(0) += 1
        false
      } else {
        true
      }

    if (s.charAt(idx(0)) != 'P') {
      throw new IllegalArgumentException(s)
    }
    idx(0) += 1

    // phase 1: chop the string into chunks
    var dateLen = 0
    val dateParts = new Array[String](3)
    val datePartsIndex = new Array[Int](3)
    while (s.length != idx(0) && BigTimeDurationValueType.isDigit(s.charAt(idx(0))) && dateLen < 3) {
      datePartsIndex(dateLen) = idx(0)
      dateParts(dateLen) = BigTimeDurationValueType.parsePiece(s, idx)
      dateLen += 1
    }

    var hasT = false
    if (s.length != idx(0)) {
      if (s.charAt(idx(0)) == 'T') {
        hasT = true
        idx(0) += 1
      } else {
        throw new IllegalArgumentException(s)
      }
    }

    var timeLen = 0
    val timeParts = new Array[String](3)
    val timePartsIndex = new Array[Int](3)
    while (s.length != idx(0) && BigTimeDurationValueType.isDigitOrPeriod(s.charAt(idx(0))) && timeLen < 3) {
      timePartsIndex(timeLen) = idx(0)
      timeParts(timeLen) = BigTimeDurationValueType.parsePiece(s, idx)
      timeLen += 1
    }

    if (s.length != idx(0)) {
      throw new IllegalArgumentException(s)
    }
    if (dateLen == 0 && timeLen == 0) {
      throw new IllegalArgumentException(s)
    }
    if (hasT && timeLen == 0) {
      throw new IllegalArgumentException(s)
    }

    // phase 2: check the ordering of chunks
    BigTimeDurationValueType.organizeParts(s, dateParts, datePartsIndex, dateLen, "YMD")
    BigTimeDurationValueType.organizeParts(s, timeParts, timePartsIndex, timeLen, "HMS")

    val parsedYear   = BigTimeDurationValueType.parseBigInteger(s, dateParts(0), datePartsIndex(0))
    val parsedMonth  = BigTimeDurationValueType.parseBigInteger(s, dateParts(1), datePartsIndex(1))
    val parsedDay    = BigTimeDurationValueType.parseBigInteger(s, dateParts(2), datePartsIndex(2))
    val parsedHour   = BigTimeDurationValueType.parseBigInteger(s, timeParts(0), timePartsIndex(0))
    val parsedMinute = BigTimeDurationValueType.parseBigInteger(s, timeParts(1), timePartsIndex(1))
    val parsedSecond = BigTimeDurationValueType.parseBigDecimal(s, timeParts(2), timePartsIndex(2))

    this.year   = if (parsedYear != null) parsedYear else BigInteger.ZERO
    this.month  = if (parsedMonth != null) parsedMonth else BigInteger.ZERO
    this.day    = if (parsedDay != null) parsedDay else BigInteger.ZERO
    this.hour   = if (parsedHour != null) parsedHour else BigInteger.ZERO
    this.minute = if (parsedMinute != null) parsedMinute else BigInteger.ZERO
    this.second = if (parsedSecond != null) parsedSecond else Util.decimal0

    if (getSignum(this.year) == 0 && getSignum(this.month) == 0 && getSignum(this.day) == 0 &&
        getSignum(this.hour) == 0 && getSignum(this.minute) == 0 && getSignum(this.second) == 0) {
      this.signum = 0
    } else if (positive) {
      this.signum = 1
    } else {
      this.signum = -1
    }
  }

  private def getSignum(i: BigInteger): Int =
    if (i == null) 0 else i.signum()

  private def getSignum(i: BigDecimal): Int =
    if (i == null) 0 else i.signum()
}

object BigTimeDurationValueType {

  private lazy val testInstance = Array(
    new BigDateTimeValueType(new BigInteger("1696"), 8 /*Sep*/, 0 /*1st*/, 0, 0, new BigDecimal(0), TimeZone.ZERO),
    new BigDateTimeValueType(new BigInteger("1697"), 1 /*Feb*/, 0 /*1st*/, 0, 0, new BigDecimal(0), TimeZone.ZERO),
    new BigDateTimeValueType(new BigInteger("1903"), 2 /*Mar*/, 0 /*1st*/, 0, 0, new BigDecimal(0), TimeZone.ZERO),
    new BigDateTimeValueType(new BigInteger("1903"), 6 /*Jul*/, 0 /*1st*/, 0, 0, new BigDecimal(0), TimeZone.ZERO)
  )

  def compare(lhs: ITimeDurationValueType, rhs: ITimeDurationValueType): Int = {
    var less = false
    var greater = false
    var noDeterminate = false

    for (inst <- testInstance) {
      val l = inst.add(lhs).asInstanceOf[BigDateTimeValueType]
      val r = inst.add(rhs).asInstanceOf[BigDateTimeValueType]

      val v = BigDateTimeValueType.compare(l, r)

      if (v < 0) less = true
      if (v > 0) greater = true
      if (v == 0 && !l.equals(r)) noDeterminate = true
    }

    if (noDeterminate) Comparator.UNDECIDABLE
    else if (less && greater) Comparator.UNDECIDABLE
    else if (less) Comparator.LESS
    else if (greater) Comparator.GREATER
    else Comparator.EQUAL
  }

  def fromMinutes(minutes: Int): BigTimeDurationValueType =
    fromMinutes(Util.int2bi(minutes))

  def fromMinutes(minutes: BigInteger): BigTimeDurationValueType =
    new BigTimeDurationValueType(minutes.signum(), null, null, null, null, minutes.abs(), null)

  private def isDigit(ch: Char): Boolean =
    '0' <= ch && ch <= '9'

  private def isDigitOrPeriod(ch: Char): Boolean =
    isDigit(ch) || ch == '.'

  private def parsePiece(whole: String, idx: Array[Int]): String = {
    val start = idx(0)
    while (idx(0) < whole.length && isDigitOrPeriod(whole.charAt(idx(0)))) {
      idx(0) += 1
    }
    if (idx(0) == whole.length) {
      throw new IllegalArgumentException(whole)
    }
    idx(0) += 1
    whole.substring(start, idx(0))
  }

  private def organizeParts(
    whole: String,
    parts: Array[String],
    partsIndex: Array[Int],
    len: Int,
    tokens: String
  ): Unit = {
    var idx = tokens.length
    for (i <- (len - 1) to 0 by -1) {
      val nidx = tokens.lastIndexOf(parts(i).charAt(parts(i).length - 1), idx - 1)
      if (nidx == -1) {
        throw new IllegalArgumentException(whole)
      }
      for (j <- (nidx + 1) until idx) {
        parts(j) = null
      }
      idx = nidx
      parts(idx) = parts(i)
      partsIndex(idx) = partsIndex(i)
    }
    idx -= 1
    while (idx >= 0) {
      parts(idx) = null
      idx -= 1
    }
  }

  private def parseBigInteger(whole: String, part: String, index: Int): BigInteger = {
    if (part == null) null
    else new BigInteger(part.substring(0, part.length - 1))
  }

  private def parseBigDecimal(whole: String, part: String, index: Int): BigDecimal = {
    if (part == null) null
    else new BigDecimal(part.substring(0, part.length - 1))
  }
}
