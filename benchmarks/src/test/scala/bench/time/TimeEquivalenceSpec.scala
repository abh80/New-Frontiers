package bench.time

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalactic.Tolerance.*

import bench.support.OrekitData
import org.abh80.nf.core.time.{AbsoluteTime, TimeScaleFactory}
import org.orekit.time.{AbsoluteDate, TimeScalesFactory}

class TimeEquivalenceSpec extends AnyFlatSpec with Matchers:
  OrekitData.ensureLoaded()
  private val tol = 1e-6

  "NF AbsoluteTime" should "match Orekit on a shifted duration (TAI)" in {
    val nf = new AbsoluteTime(2020, 3, 1, 12, 0, 0.0, TimeScaleFactory.getTAI)
    val nfDur = nf.++(3600.0).durationFrom(nf).toDouble

    val ok = new AbsoluteDate(2020, 3, 1, 12, 0, 0.0, TimeScalesFactory.getTAI())
    val okDur = ok.shiftedBy(3600.0).durationFrom(ok)

    nfDur shouldBe (okDur +- tol)
    nfDur shouldBe (3600.0 +- tol)
  }

  it should "agree on the UTC-TAI offset magnitude (37s since 2017)" in {
    val nf = new AbsoluteTime(2020, 1, 1, 0, 0, 0.0, TimeScaleFactory.getTAI)
    val nfOffset = math.abs(nf.offsetBetween(TimeScaleFactory.getUTC, TimeScaleFactory.getTAI).toDouble)

    val ok = new AbsoluteDate(2020, 1, 1, 0, 0, 0.0, TimeScalesFactory.getTAI())
    val okOffset = math.abs(TimeScalesFactory.getUTC().offsetFromTAI(ok))

    nfOffset shouldBe (okOffset +- tol)
    nfOffset shouldBe (37.0 +- tol)
  }
