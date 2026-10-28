package org.orbeon.msv.datatype.xsd.datetime

import utest.*
import java.math.BigInteger

object DateTimeTest extends TestSuite {

  val tests = Tests {
    test("Calendar validation - Year 0, leading zeros, leap years, timezone offsets") {
      val fmt = "%Y-%M-%DT%h:%m:%s%z"

      def isValid(s: String): Boolean =
        try { PreciseCalendarParser.parse(fmt, s); true } catch { case _: IllegalArgumentException => false }

      // Year 0 is invalid
      assert(!isValid("0000-01-01T00:00:00Z"))
      assert(!isValid("-0000-01-01T00:00:00Z"))

      // 5+ digit years must not have leading zero
      assert(!isValid("01999-01-01T00:00:00Z"))
      assert(isValid("0999-01-01T00:00:00Z"))
      assert(isValid("10000-01-01T00:00:00Z"))
      assert(isValid("99999999999999999999-01-01T00:00:00Z"))

      // Day in month / leap year validation
      assert(isValid("2000-02-29T00:00:00Z")) // leap year
      assert(!isValid("1900-02-29T00:00:00Z")) // non-leap century
      assert(!isValid("2023-02-29T00:00:00Z")) // non-leap year
      assert(!isValid("2023-04-31T00:00:00Z")) // April only has 30 days
      assert(isValid("2023-01-31T00:00:00Z")) // January has 31 days

      // Timezone offset range [-14:00, +14:00]
      assert(isValid("2023-01-01T00:00:00+14:00"))
      assert(isValid("2023-01-01T00:00:00-14:00"))
      assert(!isValid("2023-01-01T00:00:00+14:01"))
      assert(!isValid("2023-01-01T00:00:00-15:00"))

      // Negative year roundtrip
      val negDt = PreciseCalendarParser.parse(fmt, "-0001-01-01T00:00:00Z")
      val formattedNeg = PreciseCalendarFormatter.format(fmt, negDt)
      assert(formattedNeg == "-0001-01-01T00:00:00Z")
    }

    test("Duration parsing and formatting") {
      val dur1 = new BigTimeDurationValueType("P1Y2M3DT4H5M6S")
      assert(dur1.signum == 1)
      assert(dur1.getYear == BigInteger.valueOf(1))
      assert(dur1.getMonth == BigInteger.valueOf(2))
      assert(dur1.getDay == BigInteger.valueOf(3))
      assert(dur1.getHour == BigInteger.valueOf(4))
      assert(dur1.getMinute == BigInteger.valueOf(5))
      assert(dur1.getSecond.intValue == 6)

      val dur2 = new BigTimeDurationValueType("-P2Y")
      assert(dur2.signum == -1)
      assert(dur2.getYear == BigInteger.valueOf(2))

      // Trailing T with no time components is invalid
      val caughtT = try { new BigTimeDurationValueType("P123YT"); false } catch { case _: IllegalArgumentException => true }
      assert(caughtT)
    }

    test("DateTime parsing and formatting") {
      val dt = PreciseCalendarParser.parse("%Y-%M-%DT%h:%m:%s%z", "2026-10-27T17:30:00Z")
      assert(dt.getYear == BigInteger.valueOf(2026))
      assert(dt.getMonth.intValue() == 9) // 0-based: October is 9
      assert(dt.getDay.intValue() == 26)   // 0-based: 27 is 26
      assert(dt.getHour.intValue() == 17)
      assert(dt.getMinute.intValue() == 30)

      val formatted = PreciseCalendarFormatter.format("%Y-%M-%DT%h:%m:%s%z", dt)
      assert(formatted == "2026-10-27T17:30:00Z")
    }

    test("DateTime comparison") {
      val dt1 = PreciseCalendarParser.parse("%Y-%M-%DT%h:%m:%s%z", "2026-10-27T12:00:00Z")
      val dt2 = PreciseCalendarParser.parse("%Y-%M-%DT%h:%m:%s%z", "2026-10-27T13:00:00Z")
      val dt3 = PreciseCalendarParser.parse("%Y-%M-%DT%h:%m:%s%z", "2026-10-27T14:00:00+02:00") // equals 12:00:00Z

      assert(dt1.compare(dt2) < 0)
      assert(dt2.compare(dt1) > 0)
      assert(dt1.compare(dt3) == 0)
      assert(dt1.equals(dt3))
    }
  }
}
