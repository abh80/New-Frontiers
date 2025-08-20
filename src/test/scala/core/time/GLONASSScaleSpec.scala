package org.abh80.nf
package core.time

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class GLONASSScaleSpec extends AnyFunSuite with Matchers {
  val scale = TimeScaleFactory.getGLONASS

  test("epoch date with scale") {
    "GLONASS" shouldEqual scale.toString

    val t = AbsoluteTime(Date(1996, 1, 1), Time.MIDNIGHT, scale)
    assertResult(t)(EpochFactory.GLONASS)
  }

  test("utc offset") {
    /** There is a 3-hour difference between UTC scale and GLONASS scale */
    val ut = AbsoluteTime(2017, 12, 31, 23, 59, 59, TimeScaleFactory.getUTC)
    val gt = AbsoluteTime(2018, 1, 1, 2, 59, 59, scale)

    assertResult(ut)(gt)
  }

  test("a leap second year") {
    val t1 = AbsoluteTime(Date(2017, 1, 1), Time(2, 59, 59), scale)
    val t2 = AbsoluteTime(Date(2017, 1, 1), Time(3, 0, 1), scale)

    t2.durationFrom(t1).toDouble shouldEqual 3.0
  }

  test("not a leap second year") {
    val t1 = AbsoluteTime(Date(2010, 1, 1), Time(2, 59, 59), scale)
    val t2 = AbsoluteTime(Date(2010, 1, 1), Time(3, 0, 1), scale)

    t2.durationFrom(t1).toDouble shouldEqual 2.0
  }

  test("during a leap second") {
    var t = AbsoluteTime(Date(2009, 1, 1), Time(2, 59, 59), scale)
    assertResult("2009-01-01T02:59:59.000")(t.toString(scale))
    assertResult("2009-01-01T02:58:59.000")(t.++(-60).toString(scale))
    assertResult(false)(scale.isInsideLeapSecond(t))
    assertResult(61)(scale.minuteDuration(t))
    assertResult(60)(scale.minuteDuration(t.++(-60)))

    t = t ++ 0.263
    assertResult("2009-01-01T02:59:59.263")(t.toString(scale))
    assertResult(false)(scale.isInsideLeapSecond(t))
    assertResult(61)(scale.minuteDuration(t))

    t = t ++ 0.263
    assertResult("2009-01-01T02:59:59.526")(t.toString(scale))
    assertResult(false)(scale.isInsideLeapSecond(t))
    assertResult(61)(scale.minuteDuration(t))

    t = t ++ 0.263
    assertResult("2008-12-31T23:59:59.789Z")(t.toString)
    assertResult(false)(scale.isInsideLeapSecond(t))
    assertResult(61)(scale.minuteDuration(t))

    t = t ++ 0.263
    assertResult("2008-12-31T23:59:60.052Z")(t.toString)
    assertResult(true)(scale.isInsideLeapSecond(t))
    assertResult(61)(scale.minuteDuration(t))

    t = t ++ 0.263
    assertResult("2008-12-31T23:59:60.315Z")(t.toString)
    assertResult(true)(scale.isInsideLeapSecond(t))
    assertResult(61)(scale.minuteDuration(t))

    t = t ++ 0.263
    assertResult("2008-12-31T23:59:60.578Z")(t.toString)
    assertResult(true)(scale.isInsideLeapSecond(t))
    assertResult(61)(scale.minuteDuration(t))

    t = t ++ 0.263
    assertResult("2008-12-31T23:59:60.841Z")(t.toString)
    assertResult(true)(scale.isInsideLeapSecond(t))
    assertResult(61)(scale.minuteDuration(t))

    t = t ++ 0.263
    assertResult("2009-01-01T00:00:00.104Z")(t.toString)
    assertResult(false)(scale.isInsideLeapSecond(t))
    assertResult(60)(scale.minuteDuration(t))
  }
}
