package org.abh80.nf
package util

import org.scalatest.funsuite.AnyFunSuite
import org.abh80.nf.util.DateUtil._

class DateUtilSpec extends AnyFunSuite {

  test("ISO8601 formats date as yyyy-mm-dd") {
    val iso = ISO8601(5, 7, 2024)
    assert(iso.toString === "2024-07-05")
  }

  test("ANSI_INCITS_30_1997 formats date as mm/dd/yyyy") {
    val ansi = ANSI_INCITS_30_1997(5, 7, 2024)
    assert(ansi.toString === "07/05/2024")
  }

  test("EN28601 formats date as dd/mm/yyyy") {
    val en = EN28601(5, 7, 2024)
    assert(en.toString === "05/07/2024")
  }

  test("ASIAN formats date as yyyy.mm.dd") {
    val asian = ASIAN(5, 7, 2024)
    assert(asian.toString === "2024.07.05")
  }

  test("RFC822 formats date as Week, dd Mon yyyy") {
    val rfc = RFC822(5, 7, 2024, 1) // Monday
    assert(rfc.toString === "Mon, 05 Jul 2024")
  }

  test("WeekUtil.fromInt returns correct weekday") {
    assert(WeekUtil.fromInt(1) === Weekday.MONDAY)
    assert(WeekUtil.fromInt(7) === Weekday.SUNDAY)
    intercept[IllegalArgumentException] {
      WeekUtil.fromInt(0)
    }
  }

  test("WeekUtil.isWeekday and isWeekend") {
    assert(WeekUtil.isWeekday(Weekday.MONDAY))
    assert(!WeekUtil.isWeekend(Weekday.MONDAY))
    assert(WeekUtil.isWeekend(Weekday.SATURDAY))
    assert(WeekUtil.isWeekend(Weekday.SUNDAY))
  }

  test("Month.getMonth returns correct month") {
    assert(Month.getMonth(1) === Month.JANUARY)
    assert(Month.getMonth(12) === Month.DECEMBER)
    intercept[IllegalArgumentException] {
      Month.getMonth(0)
    }
  }

  test("Month.getIntegerValue returns correct integer") {
    assert(Month.JANUARY.getIntegerValue === 1)
    assert(Month.DECEMBER.getIntegerValue === 12)
  }
}

