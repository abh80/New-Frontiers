package org.abh80.nf
package core.time

import util.DateUtil.Month

import org.scalatest.funsuite.AnyFunSuite

import java.time.Instant

class DateSpec extends AnyFunSuite {
  test("reference days") {
    val referenceDays: Array[(Int, Int, Int, Int)] = Array(
      (2000, 1, 1, 0),
      (2000, 3, 1, 60),
      (1858, 11, 16, -51545),
      (-4713, 12, 31, -2451546),
      (-4712, 1, 1, -2451545),
      (1582, 10, 4, -152385), // Last day of Julian calendar
      (1582, 10, 15, -152384), // First day of Gregorian calendar
      (1600, 1, 1, -146097), // Gregorian leap year
      (1900, 1, 1, -36524), // Non-leap century year
      (2000, 2, 29, 59), // Leap year
      (1999, 12, 31, -1), // Day before J2000
      (1, 1, 3, -730119), // Start of Julian calendar
      (0, 1, 4, -730484), // Year zero (proleptic)
      (-1000, 1, 3, -1095735), // Far negative year
      (0, 12, 31, -730122),
      (1, 1, 1, -730121)
    )

    for (i <- referenceDays) {
      val day = Date(Date.J2000_0, i._4)
      assertResult(i._1)(day.getYear)
      assertResult(i._2)(day.getMonth)
      assertResult(i._3)(day.getDay)
    }
  }

  test("days of week") {
    assertResult(1)(Date(-4712, 1, 1).getDayOfWeek)
    assertResult(1)(Date(-4713, 1, 2).getDayOfWeek)
    assertResult(6)(Date(2000, 1, 1).getDayOfWeek)
    assertResult(4)(Date(1582, 10, 4).getDayOfWeek)
    assertResult(3)(Date(1, 1, 5).getDayOfWeek)
    assertResult(6)(Date(1582, 10, 16).getDayOfWeek)
    assertResult(7)(Date(1582, 10, 17).getDayOfWeek)
    assertResult(2)(Date(2025, 7, 29).getDayOfWeek)
  }

  test("day of year") {
    assertResult(1)(Date(2000, 1, 1).getDayOfYear)
    assertResult(365)(Date(1999, 12, 31).getDayOfYear)
    assertResult(366)(Date(2000, 12, 31).getDayOfYear)
    assertResult(60)(Date(2004, 2, 29).getDayOfYear)
    assertResult(60)(Date(2001, 3, 1).getDayOfYear)
    assertResult(61)(Date(2008, 3, 1).getDayOfYear)
  }

  test("month") {
    assertResult(-152388)(Date(1582, Month.OCTOBER, 1).getJ2000Day)
    assertResult(7)(Month.JULY.getIntegerValue)
    assertResult(59)(Date(2000, Month.FEBRUARY, 29).getJ2000Day)
    assertThrows[IllegalArgumentException](Date(2001, 2, 29)) // invalid date
  }

  test("sequential test") {
    var s = -2460000
    val n = 20000

    while (s <= n) {
      val d1 = Date(s)
      assertResult(s)(d1.getJ2000Day)

      val d2 = Date(d1.getYear, d1.getMonth, d1.getDay)
      assertResult(s)(d2.getJ2000Day)
      s += 1
    }
  }

  test("date comparisons") {
    val dates = Array(
      (Date(2003, 1, 1), Date(2003, 1, 1)),
      (Date(2003, 2, 28), Date(2003, 2, 28)),
      (Date(2003, 3, 1), Date(2003, 3, 1)),
      (Date(2003, 9, 26), Date(2003, 9, 26)),
      (Date(2003, 12, 31), Date(2003, 12, 31)),
      (Date(2004, 2, 28), Date(2004, 2, 28)),
      (Date(2004, 2, 29), Date(2004, 2, 29)),
      (Date(2004, 3, 1), Date(2004, 3, 1)),
      (Date(2004, 12, 31), Date(2004, 12, 31))
    )

    for (i <- dates.indices; j <- dates.indices) {
      val (d1, d2) = (dates(i)._1, dates(j)._2)
      if (d1.compareTo(d2) < 0) {
        assert(d2.compareTo(d1) > 0)
        assert(d1 != d2)
        assert(d2 != d1)
        assert(d1.hashCode != d2.hashCode)
        assert(i < j)
      } else if (d1.compareTo(d2) > 0) {
        assert(d2.compareTo(d1) < 0)
        assert(d1 != d2)
        assert(d2 != d1)
        assert(d1.hashCode != d2.hashCode)
        assert(i > j)
      } else {
        assert(d2.compareTo(d1) == 0)
        assert(d1 == d2)
        assert(d2 == d1)
        assert(d1.hashCode == d2.hashCode)
        assert(i == j)
      }
    }
    assert(dates(0)._1 != this)
  }

  test("toString") {
    import util.DateUtil.{ANSI_INCITS_30_1997, ASIAN, EN28601}
    // Default ISO8601 format
    assertResult("2000-01-01")(Date(2000, 1, 1).toString)
    assertResult("2004-02-29")(Date(2004, 2, 29).toString)
    assertResult("-4712-01-01")(Date(-4712, 1, 1).toString)
    assertResult("1582-10-15")(Date(1582, 10, 15).toString)

    // Custom format
    assertResult("01/01/2000")(Date(2000, 1, 1).toString(EN28601.apply))
    assertResult("2004.02.29")(Date(2004, 2, 29).toString(ASIAN.apply))
    assertResult("10/15/1582")(Date(1582, 10, 15).toString(ANSI_INCITS_30_1997.apply))
  }

  test("get week") {
    assertResult(52)(Date(1995, 1, 1).getWeek)
    assertResult(52)(Date(1996, 12, 29).getWeek)
    assertResult(1)(Date(1996, 12, 30).getWeek)
  }

  test("ISO-8601 week-number edge cases") {
    assertResult(1)(Date(2015, 1, 1).getWeek)
    assertResult(53)(Date(2010, 1, 3).getWeek)
    assertResult(53)(Date(2009, 12, 31).getWeek)

    // --- 3. Years that actually have week 53 (leap-week years) ---
    assertResult(53)(Date(2009, 12, 31).getWeek) // 2009 has week 53.
    assertResult(53)(Date(2015, 12, 28).getWeek) // Monday of ISO week 53 in 2015.
    assertResult(53)(Date(2020, 12, 31).getWeek) // Leap year with week 53.

    // --- 4. First Monday of the year must be week 1, even if it is 31 Dec of previous year ---
    assertResult(1)(Date(2007, 1, 1).getWeek) // Monday; 2007-01-01 is week 1 by definition.

    // --- 5. Middle-of-year sanity check (should not be week 1/52/53) ---
    assertResult(27)(Date(2024, 7, 3).getWeek) // Mid-year random date.

    // --- 6. Minimum/maximum representable dates (if your Date class supports them) ---
    // Adjust years if implementation limits differ.
    assertResult(52)(Date(1900, 12, 30).getWeek) // Day before 1900-12-31 Monday (ISO week 1 of 1901).
    assertResult(1)(Date(1901, 1, 1).getWeek) // Verify rollover at lower bound.
  }

  test("construct Date from Instant") {
    val instant = Instant.parse("2000-01-01T00:00:00Z")
    val date = new Date(instant)
    assertResult(2000)(date.getYear)
    assertResult(1)(date.getMonth)
    assertResult(1)(date.getDay)

    val instantBeforeJ2000 = Instant.parse("1999-12-31T23:59:59Z")
    val dateBeforeJ2000 = new Date(instantBeforeJ2000)
    assertResult(1999)(dateBeforeJ2000.getYear)
    assertResult(12)(dateBeforeJ2000.getMonth)
    assertResult(31)(dateBeforeJ2000.getDay)
  }

  test("mjd") {
    assertResult(0)(Date.MJD.getMJD)
    assertResult(37665)(Date(1962, 1,  1).getMJD)
    assertResult(35)(Date(Date.MJD, 35).getMJD)
  }
}
