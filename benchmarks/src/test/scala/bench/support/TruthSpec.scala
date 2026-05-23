package bench.support

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalactic.Tolerance.*

import org.abh80.nf.core.time.AbsoluteTime
import org.abh80.nf.core.time.TimeScaleFactory

class TruthSpec extends AnyFlatSpec with Matchers:
  // `new AbsoluteTime()` is not zero-offset internally. NF's timeline is TAI-based, so J2000
  // noon-TT sits at -(TT-TAI) = -32.184 s. The helper extracts exact elapsed seconds, so test
  // a difference between two times, which is base-independent.
  "Truth.nfExactSeconds" should "extract exact elapsed seconds between two times" in {
    val t1 = new AbsoluteTime()
    val t2 = t1.++(10.0)
    (Truth.nfExactSeconds(t2) - Truth.nfExactSeconds(t1)).toDouble shouldBe (10.0 +- 1e-12)
  }

  it should "expose defined scale offsets" in {
    Truth.TT_MINUS_TAI shouldBe (32.184 +- 1e-12)
    Truth.TAI_MINUS_UTC_SINCE_2017 shouldBe (37.0 +- 1e-12)
    Truth.GPS_MINUS_TAI shouldBe (-19.0 +- 1e-12)
  }
