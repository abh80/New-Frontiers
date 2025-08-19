package org.abh80.nf
package core.time

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class TTScaleTest extends AnyFunSuite with Matchers {

  test("TTScale should return correct timePastTAI") {
    val ttScale = new TTScale()
    val absoluteTime = new AbsoluteTime(new Date(2023, 10, 1), new Time(12, 0, 0), TAIScale())
    val expectedOffset = TimeFormat.fromTimeUnit(32L, TimeUnit.SECONDS) + TimeFormat.fromTimeUnit(184L, TimeUnit.MILLISECONDS)

    ttScale.timePastTAI(absoluteTime) shouldEqual expectedOffset
    ttScale.timePastTAI(absoluteTime) shouldEqual(TDTScale().timePastTAI(absoluteTime))
  }

  test("TTScale should return correct timeToTAI") {
    val ttScale = new TTScale()
    val date = new Date(2023, 10, 1)
    val time = new Time(12, 0, 0)
    val expectedOffset = (TimeFormat.fromTimeUnit(32L, TimeUnit.SECONDS) + TimeFormat.fromTimeUnit(184L, TimeUnit.MILLISECONDS)).negate()

    ttScale.timeToTAI(date, time) shouldEqual expectedOffset
  }
}