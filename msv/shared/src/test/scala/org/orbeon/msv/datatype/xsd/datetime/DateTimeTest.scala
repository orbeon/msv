package org.orbeon.msv.datatype.xsd.datetime

import utest.*
import java.math.BigInteger

object DateTimeTest extends TestSuite {

  val tests = Tests {
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
