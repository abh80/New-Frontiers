package bench.precision

import bench.support.{OrekitData, PrecisionResult, Truth}
import org.abh80.nf.core.time.AbsoluteTime
import org.orekit.time.AbsoluteDate

/** Precision via large-count accumulation and defined-offset agreement. */
object TimePrecisionChecks:
  private val cat = "Time"

  def run(): Seq[PrecisionResult] =
    OrekitData.ensureLoaded()
    val n = 10_000_000
    val step = 0.1
    val truth = BigDecimal(n) * BigDecimal("0.1") // = 1_000_000 exactly

    // NF accumulation. `new AbsoluteTime()` is NOT zero-offset internally (TAI-based timeline,
    // base = -32.184 s), so measure elapsed relative to the base.
    val nfBase = new AbsoluteTime()
    var nf = nfBase
    var i = 0
    while i < n do { nf = nf.++(step); i += 1 }
    val nfElapsed = Truth.nfExactSeconds(nf) - Truth.nfExactSeconds(nfBase)
    val nfErr = Truth.absError(nfElapsed, truth)

    // Orekit accumulation
    var ok = AbsoluteDate.J2000_EPOCH
    i = 0
    while i < n do { ok = ok.shiftedBy(step); i += 1 }
    val okErr = Truth.absError(ok.durationFrom(AbsoluteDate.J2000_EPOCH), truth)

    Seq(
      PrecisionResult(cat, "accumulate_0.1s_x10M", "nf", nfErr),
      PrecisionResult(cat, "accumulate_0.1s_x10M", "ok", okErr)
    )
