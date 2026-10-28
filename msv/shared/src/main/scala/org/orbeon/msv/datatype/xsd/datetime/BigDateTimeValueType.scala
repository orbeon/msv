/*
 * @(#)$Id: BigDateTimeValueType.java,v 1.22 2005/03/31 23:19:07 kohsuke Exp $
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
 * DateTimeValueType object that can hold all lexically valid dateTime value.
 * 
 * This class provides:
 * <ol>
 *  <li> Unlimited digits for year (e.g., "year 9999999999999999999999")
 *  <li> Unlimited digits for fraction of second (e.g. 0.00000000000001 sec)
 * </ol>
 * 
 * To provide methods that can change date/time values, normalize method
 * should be modified too.
 * 
 * @author Kohsuke KAWAGUCHI
 */
class BigDateTimeValueType(
  private var _year: BigInteger,
  private var _month: java.lang.Integer,
  private var _day: java.lang.Integer,
  private var _hour: java.lang.Integer,
  private var _minute: java.lang.Integer,
  private var _second: BigDecimal,
  private var _zone: TimeZone
) extends IDateTimeValueType {

  def this() =
    this(null, null, null, null, null, null, null)

  def this(base: BigDateTimeValueType, newTimeZone: TimeZone) =
    this(base.getYear, base.getMonth, base.getDay, base.getHour, base.getMinute, base.getSecond, newTimeZone)

  def this(
    year: BigInteger,
    month: Int,
    day: Int,
    hour: Int,
    minute: Int,
    second: BigDecimal,
    timeZone: TimeZone
  ) =
    this(
      year,
      java.lang.Integer.valueOf(month),
      java.lang.Integer.valueOf(day),
      java.lang.Integer.valueOf(hour),
      java.lang.Integer.valueOf(minute),
      second,
      timeZone
    )

  def getYear: BigInteger = _year
  def getMonth: java.lang.Integer = _month
  def getDay: java.lang.Integer = _day
  def getHour: java.lang.Integer = _hour
  def getMinute: java.lang.Integer = _minute
  def getSecond: BigDecimal = _second
  def getTimeZone: TimeZone = _zone

  def getBigValue(): BigDateTimeValueType = this

  override def equals(o: Any): Boolean =
    o match {
      case oTyped: IDateTimeValueType => equals(oTyped)
      case _ => false
    }

  def equals(rhs: IDateTimeValueType): Boolean = {
    val other = if (rhs.isInstanceOf[BigDateTimeValueType]) rhs.asInstanceOf[BigDateTimeValueType] else rhs.getBigValue()
    BigDateTimeValueType.compare(this, other) == Comparator.EQUAL
  }

  override def toString: String =
    PreciseCalendarFormatter.format("%Y-%M-%DT%h:%m:%s%z", this)

  override def hashCode(): Int = {
    val n = this.normalize().asInstanceOf[BigDateTimeValueType]
    Util.objHashCode(n.getYear) +
      Util.objHashCode(n.getMonth) +
      Util.objHashCode(n.getDay) +
      Util.objHashCode(n.getHour) +
      Util.objHashCode(n.getMinute) +
      Util.objHashCode(n.getSecond) +
      Util.objHashCode(n.getTimeZone)
  }

  def compare(o: IDateTimeValueType): Int = {
    val other = if (o.isInstanceOf[BigDateTimeValueType]) o.asInstanceOf[BigDateTimeValueType] else o.getBigValue()
    BigDateTimeValueType.compare(this, other)
  }

  private var normalizedValue: IDateTimeValueType = null

  def normalize(): IDateTimeValueType = {
    if (_zone == TimeZone.ZERO || _zone == null)
      return this

    if (normalizedValue != null)
      return normalizedValue

    normalizedValue = this.add(BigTimeDurationValueType.fromMinutes(-_zone.minutes))
    normalizedValue.asInstanceOf[BigDateTimeValueType]._zone = TimeZone.ZERO
    normalizedValue
  }

  def add(_rhs: ITimeDurationValueType): IDateTimeValueType = {
    if (_rhs.isInstanceOf[BigTimeDurationValueType]) {
      val rhs = _rhs.asInstanceOf[BigTimeDurationValueType]

      var quoAndMod = BigDateTimeValueType.divideAndRemainder(
        Util.int2bi(this._month).add(signed(rhs, rhs.month)),
        Util.the12
      )

      val omonth = quoAndMod(1).intValue()
      val oyear = quoAndMod(0).add(BigDateTimeValueType.nullAs0(this._year)).add(signed(rhs, rhs.year))

      val sec = BigDateTimeValueType.nullAs0(this._second).add(signed(rhs, rhs.second))

      quoAndMod = BigDateTimeValueType.divideAndRemainder(
        sec.unscaledValue(),
        Util.the60.multiply(Util.the10.pow(sec.scale()))
      )

      val osecond = new BigDecimal(quoAndMod(1), sec.scale())

      quoAndMod = BigDateTimeValueType.divideAndRemainder(
        quoAndMod(0).add(Util.int2bi(this._minute)).add(signed(rhs, rhs.minute)),
        Util.the60
      )
      val ominute = quoAndMod(1).intValue()

      quoAndMod = BigDateTimeValueType.divideAndRemainder(
        quoAndMod(0).add(Util.int2bi(this._hour)).add(signed(rhs, rhs.hour)),
        Util.the24
      )
      val ohour = quoAndMod(1).intValue()

      val md = Util.maximumDayInMonthFor(oyear, omonth)
      val dayValue = if (this._day != null) this._day.intValue() else 0
      val tempDays =
        if (dayValue < 0) 0
        else if (dayValue >= md) md - 1
        else dayValue

      var oday = signed(rhs, rhs.day).add(quoAndMod(0)).add(Util.int2bi(tempDays))
      var curMonth = omonth
      var curYear = oyear
      var done = false
      while (!done) {
        if (oday.signum() == -1) {
          oday = oday.add(Util.int2bi(Util.maximumDayInMonthFor(curYear, (curMonth + 11) % 12)))
          curMonth -= 1
        } else {
          val bmd = Util.int2bi(Util.maximumDayInMonthFor(curYear, curMonth))
          if (oday.compareTo(bmd) >= 0) {
            oday = oday.subtract(bmd)
            curMonth += 1
          } else {
            done = true
          }
        }
        if (!done) {
          if (curMonth < 0) {
            curMonth += 12
            curYear = curYear.subtract(BigInteger.ONE)
          }
          curYear = curYear.add(Util.int2bi(curMonth / 12))
          curMonth %= 12
        }
      }

      new BigDateTimeValueType(
        if (this._year != null) curYear else null,
        if (this._month != null) java.lang.Integer.valueOf(curMonth) else null,
        if (this._day != null) java.lang.Integer.valueOf(oday.intValue()) else null,
        if (this._hour != null) java.lang.Integer.valueOf(ohour) else null,
        if (this._minute != null) java.lang.Integer.valueOf(ominute) else null,
        if (this._second != null) osecond else null,
        this._zone
      )
    } else {
      add(_rhs.getBigValue())
    }
  }

  private def signed(dur: BigTimeDurationValueType, i: BigInteger): BigInteger =
    if (dur.signum < 0) i.negate() else i

  private def signed(dur: BigTimeDurationValueType, i: BigDecimal): BigDecimal =
    if (dur.signum < 0) i.negate() else i

}

object BigDateTimeValueType {

  def compare(lhsIn: BigDateTimeValueType, rhsIn: BigDateTimeValueType): Int = {
    val lhs = lhsIn.normalize().asInstanceOf[BigDateTimeValueType]
    val rhs = rhsIn.normalize().asInstanceOf[BigDateTimeValueType]

    if ((lhs._zone != null && rhs._zone != null) || (lhs._zone == null && rhs._zone == null)) {
      if (!Util.objEqual(lhs._year, rhs._year)) return Util.objCompare(lhs._year, rhs._year)
      if (!Util.objEqual(lhs._month, rhs._month)) return Util.objCompare(lhs._month, rhs._month)
      if (!Util.objEqual(lhs._day, rhs._day)) return Util.objCompare(lhs._day, rhs._day)
      if (!Util.objEqual(lhs._hour, rhs._hour)) return Util.objCompare(lhs._hour, rhs._hour)
      if (!Util.objEqual(lhs._minute, rhs._minute)) return Util.objCompare(lhs._minute, rhs._minute)
      if (!Util.objEqual(lhs._second, rhs._second)) return Util.objCompare(lhs._second, rhs._second)
      return Comparator.EQUAL
    }

    if (lhs._zone == null) {
      val r1 = compare(new BigDateTimeValueType(lhs, Util.timeZoneNeg14).normalize().asInstanceOf[BigDateTimeValueType], rhs)
      if (r1 == Comparator.EQUAL || r1 == Comparator.LESS) return Comparator.LESS

      val r2 = compare(new BigDateTimeValueType(lhs, Util.timeZonePos14).normalize().asInstanceOf[BigDateTimeValueType], rhs)
      if (r2 == Comparator.EQUAL || r2 == Comparator.GREATER) return Comparator.GREATER

      Comparator.UNDECIDABLE
    } else {
      val r1 = compare(lhs, new BigDateTimeValueType(rhs, Util.timeZonePos14))
      if (r1 == Comparator.EQUAL || r1 == Comparator.LESS) return Comparator.LESS

      val r2 = compare(lhs, new BigDateTimeValueType(rhs, Util.timeZoneNeg14))
      if (r2 == Comparator.EQUAL || r2 == Comparator.GREATER) return Comparator.GREATER

      Comparator.UNDECIDABLE
    }
  }

  private[datetime] def nullAs0(o: BigInteger): BigInteger =
    if (o != null) o else BigInteger.ZERO

  private[datetime] def nullAs0(o: BigDecimal): BigDecimal =
    if (o != null) o else Util.decimal0

  private[datetime] def divideAndRemainder(x1: BigInteger, x2: BigInteger): Array[BigInteger] = {
    val r = x1.divideAndRemainder(x2)
    if (r(1).signum() < 0) {
      r(1) = r(1).add(x2)
      r(0) = r(0).subtract(BigInteger.ONE)
    }
    r
  }
}
