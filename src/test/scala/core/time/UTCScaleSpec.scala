package org.abh80.nf
package core.time

import matchers.AlmostEqualsMatcher.almostEquals

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class UTCScaleSpec extends AnyFunSuite with Matchers {
  private final val UTC = TimeScaleFactory.getUTC

  test("toString") {
    // to hit test coverage
    UTC.getName shouldBe "UTC"
  }

  test("offsets") {
    checkOffset(1961, 1, 2, -(1.422818 + 1 * 0.001296))
    checkOffset(1961, 8, 2, -(1.372818 + 213 * 0.001296)); // MJD 37300 + 213
    checkOffset(1962, 1, 2, -(1.845858 + 1 * 0.0011232)); // MJD 37665 +   1
    checkOffset(1963, 11, 2, -(1.945858 + 670 * 0.0011232)); // MJD 37665 + 670
    checkOffset(1964, 1, 2, -(3.240130 - 365 * 0.001296)); // MJD 38761 - 365
    checkOffset(1964, 4, 2, -(3.340130 - 274 * 0.001296)); // MJD 38761 - 274
    checkOffset(1964, 9, 2, -(3.440130 - 121 * 0.001296)); // MJD 38761 - 121
    checkOffset(1965, 1, 2, -(3.540130 + 1 * 0.001296)); // MJD 38761 +   1
    checkOffset(1965, 3, 2, -(3.640130 + 60 * 0.001296)); // MJD 38761 +  60
    checkOffset(1965, 7, 2, -(3.740130 + 182 * 0.001296)); // MJD 38761 + 182
    checkOffset(1965, 9, 2, -(3.840130 + 244 * 0.001296)); // MJD 38761 + 244
    checkOffset(1966, 1, 2, -(4.313170 + 1 * 0.002592)); // MJD 39126 +   1
    checkOffset(1968, 2, 2, -(4.213170 + 762 * 0.002592)); // MJD 39126 + 762

    checkOffset(1972, 3, 5, -10)
    checkOffset(1972, 7, 14, -11)
    checkOffset(1979, 12, 31, -18)
    checkOffset(1980, 1, 22, -19)
    checkOffset(2006, 7, 7, -33)
    checkOffset(2025, 1, 12, -37)
  }

  test("not a leap second year") {
    val t1 = AbsoluteTime(Date(2015, 12, 31), Time(23, 59, 59), UTC)
    val t2 = AbsoluteTime(Date(2016, 1, 1), Time(0, 0, 1), UTC)

    t2.durationFrom(t1).toDouble shouldBe 2.0
  }

  test("a leap second year") {
    val t1 = AbsoluteTime(Date(2005, 12, 31), Time(23, 59, 59), UTC)
    val t2 = AbsoluteTime(Date(2006, 1, 1), Time(0, 0, 1), UTC)

    t2.durationFrom(t1).toDouble shouldBe 3.0
  }

  test("during a leap second") {
    var t = AbsoluteTime(Date(2008, 12, 31), Time(23, 59, 59), UTC)
    assertResult("2008-12-31T23:59:59.000Z")(t.toString)
    assertResult("2008-12-31T23:58:59.000Z")(t.++(-60).toString)
    assertResult(false)(UTC.isInsideLeapSecond(t))
    assertResult(61)(UTC.minuteDuration(t))
    assertResult(60)(UTC.minuteDuration(t.++(-60)))

    t = t ++ 0.263
    assertResult("2008-12-31T23:59:59.263Z")(t.toString)
    assertResult(false)(UTC.isInsideLeapSecond(t))
    assertResult(61)(UTC.minuteDuration(t))

    t = t ++ 0.263
    assertResult("2008-12-31T23:59:59.526Z")(t.toString)
    assertResult(false)(UTC.isInsideLeapSecond(t))
    assertResult(61)(UTC.minuteDuration(t))

    t = t ++ 0.263
    assertResult("2008-12-31T23:59:59.789Z")(t.toString)
    assertResult(false)(UTC.isInsideLeapSecond(t))
    assertResult(61)(UTC.minuteDuration(t))

    t = t ++ 0.263
    assertResult("2008-12-31T23:59:60.052Z")(t.toString)
    assertResult(true)(UTC.isInsideLeapSecond(t))
    assertResult(61)(UTC.minuteDuration(t))

    t = t ++ 0.263
    assertResult("2008-12-31T23:59:60.315Z")(t.toString)
    assertResult(true)(UTC.isInsideLeapSecond(t))
    assertResult(61)(UTC.minuteDuration(t))

    t = t ++ 0.263
    assertResult("2008-12-31T23:59:60.578Z")(t.toString)
    assertResult(true)(UTC.isInsideLeapSecond(t))
    assertResult(61)(UTC.minuteDuration(t))

    t = t ++ 0.263
    assertResult("2008-12-31T23:59:60.841Z")(t.toString)
    assertResult(true)(UTC.isInsideLeapSecond(t))
    assertResult(61)(UTC.minuteDuration(t))

    t = t ++ 0.263
    assertResult("2009-01-01T00:00:00.104Z")(t.toString)
    assertResult(false)(UTC.isInsideLeapSecond(t))
    assertResult(60)(UTC.minuteDuration(t))
  }

  test("before leap second offset") {
    val t = AbsoluteTime(1950, 1, 1, UTC)
    assertResult(0.0)(UTC.timePastTAI(t).toDouble)
    assertResult(false)(UTC.isInsideLeapSecond(t))
  }

  private def checkOffset(year: Int, month: Int, day: Int, offset: Double): Unit =
    val time = new AbsoluteTime(year, month, day, UTC)
    offset should be(almostEquals(UTC.timePastTAI(time).toDouble))
}
