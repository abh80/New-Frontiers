package org.abh80.nf
package core.time

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class IRNSSScaleSpec extends AnyFunSuite with Matchers {
  test("utc drift from IRNSS") {
    /** @see [[GPSScaleSpec]] */
    val IRNSS = TimeScaleFactory.getIRNSS
    IRNSS.toString shouldEqual "IRNSS"

    val ut = AbsoluteTime(2017, 12, 31, 23, 59, 59, TimeScaleFactory.getUTC)
    val it = AbsoluteTime(2018, 1, 1, 0, 0, 17, IRNSS)
    
    assertResult(ut)(it)
  }
}
