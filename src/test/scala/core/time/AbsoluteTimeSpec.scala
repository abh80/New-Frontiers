package org.abh80.nf
package core.time

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import java.time.Instant

class AbsoluteTimeSpec extends AnyFunSuite with Matchers {
  test("leap date") {
    var t: AbsoluteTime = AbsoluteTime(2008, 12, 31, 23, 59, 59.0, TimeScaleFactory.getUTC)
    assertResult("2008-12-31T23:59:59.000Z")(t.toString) 
    
    t = t ++ 0.625
    assertResult("2008-12-31T23:59:59.625Z")(t.toString)
    
    t = t ++ 0.625
    assertResult("2008-12-31T23:59:60.250Z")(t.toString)

    t = t ++ 0.750
    assertResult("2009-01-01T00:00:00.000Z")
  }

  test("scales offset") {
    val t = AbsoluteTime(2017, 2, 24, 15, 38, 0, TimeScaleFactory.getUTC)
    assertResult(37)(t.offsetBetween(TimeScaleFactory.getTAI, TimeScaleFactory.getUTC).toDouble)
  }

  test("from instant 1970") {
    val t = AbsoluteTime(Instant.ofEpochSecond(0L))
    assertResult("1970-01-01T00:00:00.000Z")(t.toString)
    assertResult(EpochFactory.UNIX)(t)
  }
}
