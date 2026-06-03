package org.abh80.nf
package util

import core.time.TimeUnit
import frames.DefaultFrames
import org.scalatest.funsuite.AnyFunSuite

/**
 * Scala 2.13 mirror specs: the per-version reimplementations of `enum` (sealed abstract class +
 * case object + manual `values` / `fromOrdinal`) must expose the same API surface the Scala 3
 * enum auto-derives. Lives under `src/test/scala-2.13/` so it only runs on 2.13.
 */
class MirrorParitySpec extends AnyFunSuite {

  test("Weekday.values lists all 7 days in declaration order") {
    val expected = Array(
      Weekday.MONDAY, Weekday.TUESDAY, Weekday.WEDNESDAY,
      Weekday.THURSDAY, Weekday.FRIDAY, Weekday.SATURDAY, Weekday.SUNDAY
    )
    assertResult(expected.toSeq)(Weekday.values.toSeq)
  }

  test("Weekday.fromOrdinal round-trips against values.indexOf") {
    Weekday.values.zipWithIndex.foreach { case (day, idx) =>
      assertResult(day)(Weekday.fromOrdinal(idx))
    }
  }

  test("Month.values lists all 12 months in declaration order") {
    val expected = Array(
      Month.JANUARY, Month.FEBRUARY, Month.MARCH, Month.APRIL,
      Month.MAY, Month.JUNE, Month.JULY, Month.AUGUST,
      Month.SEPTEMBER, Month.OCTOBER, Month.NOVEMBER, Month.DECEMBER
    )
    assertResult(expected.toSeq)(Month.values.toSeq)
  }

  test("Month.fromOrdinal round-trips for every value") {
    Month.values.zipWithIndex.foreach { case (m, idx) =>
      assertResult(m)(Month.fromOrdinal(idx))
    }
  }

  test("Month.asInt matches the 1-12 calendar value") {
    Month.values.zipWithIndex.foreach { case (m, idx) =>
      assertResult(idx + 1)(m.asInt)
    }
  }

  test("TimeUnit.values exposes the full descending-magnitude set") {
    val expected = Array(
      TimeUnit.DAYS, TimeUnit.HOURS, TimeUnit.MINUTES, TimeUnit.SECONDS,
      TimeUnit.MILLISECONDS, TimeUnit.MICROSECONDS, TimeUnit.NANOSECONDS,
      TimeUnit.PICOSECONDS, TimeUnit.FEMTOSECONDS, TimeUnit.ATTOSECONDS
    )
    assertResult(expected.toSeq)(TimeUnit.values.toSeq)
  }

  test("DefaultFrames.values exposes the GCRF singleton") {
    assertResult(Seq(DefaultFrames.GCRF))(DefaultFrames.values.toSeq)
    assertResult("GCRF")(DefaultFrames.GCRF.getName)
  }
}
