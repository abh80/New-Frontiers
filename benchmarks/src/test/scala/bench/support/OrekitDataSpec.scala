package bench.support

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalactic.Tolerance.*

import org.orekit.time.{AbsoluteDate, TimeScalesFactory}

class OrekitDataSpec extends AnyFlatSpec with Matchers:
  "OrekitData" should "load leap-second data so UTC is usable" in {
    OrekitData.ensureLoaded()
    val utc = TimeScalesFactory.getUTC()
    // TAI - UTC = 37s since 2017-01-01 (IERS). offsetFromTAI(UTC) is therefore -37.
    val date = new AbsoluteDate(2017, 1, 1, 0, 0, 0.0, utc)
    utc.offsetFromTAI(date) shouldBe (-37.0 +- 1e-9)
  }
