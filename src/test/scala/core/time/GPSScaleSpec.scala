package org.abh80.nf
package core.time

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class GPSScaleSpec extends AnyFunSuite with Matchers {
  val GPS = TimeScaleFactory.getGPS

  test("epoch date with scale") {
    GPS.toString shouldEqual "GPS"
    val t = AbsoluteTime(Date(1980, 1, 6), Time.MIDNIGHT, GPS)
    assertResult(EpochFactory.GPS)(t)
  }

  test("utc drift from gps") {
    /* Over time the UTC will appear slower than GPS, since GPS is fixedly offset from UTC at Midnight 1980, January, 6. */
    /* For this case, by 2017 a total of 37 leap seconds have been added, so UTC = TAI - 37 and GPS = TAI - 19. Therefore, GPS = UTC + 18 an 2017. */
    val ut = AbsoluteTime(2017, 12, 31, 23, 59, 59, TimeScaleFactory.getUTC)
    val gt = AbsoluteTime(2018, 1, 1, 0, 0, 17, GPS)

    assertResult(ut)(gt)
  }
}
