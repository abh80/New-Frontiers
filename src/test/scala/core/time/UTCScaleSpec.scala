package org.abh80.nf
package core.time

import matchers.AlmostEqualsMatcher.almostEquals

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class UTCScaleSpec extends AnyFunSuite with Matchers {
  private final val UTC = TimeScaleFactory.getUTC
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

    checkOffset(1972, 3, 5, -10);
    checkOffset(1972, 7, 14, -11);
    checkOffset(1979, 12, 31, -18);
    checkOffset(1980, 1, 22, -19);
    checkOffset(2006, 7, 7, -33);
  }

  private def checkOffset(year: Int, month: Int, day: Int, offset: Double): Unit =
    val time = new AbsoluteTime(year, month, day, UTC)
    offset should be(almostEquals(UTC.timePastTAI(time).toDouble))
}
