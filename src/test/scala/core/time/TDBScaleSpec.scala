package org.abh80.nf
package core.time

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import matchers.AlmostEqualsMatcher.almostEquals

class TDBScaleSpec extends AnyFunSuite with Matchers {
  test("reference") {
    val scale = TimeScaleFactory.getTDB
    scale.timePastTAI(EpochFactory.J2000_0).toDouble should be(almostEquals(32.183927340791372839, 1.0e-15))
  }

  test("shifted 10000000") {
    val scale = TimeScaleFactory.getTDB
    val testTime = EpochFactory.J2000_0 ++ 10000000L
    scale.timePastTAI(testTime).toDouble should be(almostEquals(32.18553194051706))
  }

  test("test time to tai 10000000") {
    val scale = TimeScaleFactory.getTDB
    val testTime = AbsoluteTime(2000, 4, 26, 5, 46, 40.001531940517253971, scale)
    val s = EpochFactory.J2000_0.++(10000000L).durationFrom(testTime).toDouble

    s should be(almostEquals(0.0))
  }
}
