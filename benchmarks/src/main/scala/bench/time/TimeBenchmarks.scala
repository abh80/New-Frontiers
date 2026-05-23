package bench.time

import org.openjdk.jmh.annotations.*
import java.util.concurrent.TimeUnit

import bench.support.OrekitData
import org.abh80.nf.core.time.{AbsoluteTime, TimeScaleFactory}
import org.orekit.time.{AbsoluteDate, TimeScalesFactory, TimeScale as OkTimeScale}

@State(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@Fork(1)
@Warmup(iterations = 5, time = 1)
@Measurement(iterations = 5, time = 1)
class TimeBenchmarks:
  var nfA: AbsoluteTime = new AbsoluteTime()
  var nfB: AbsoluteTime = new AbsoluteTime()
  var okA: AbsoluteDate = AbsoluteDate.J2000_EPOCH
  var okB: AbsoluteDate = AbsoluteDate.J2000_EPOCH
  var okTai: OkTimeScale = null
  var okUtc: OkTimeScale = null

  @Setup(Level.Trial)
  def setup(): Unit =
    OrekitData.ensureLoaded()
    nfA = new AbsoluteTime(2020, 3, 1, 12, 0, 0.0, TimeScaleFactory.getTAI)
    nfB = new AbsoluteTime(2021, 6, 15, 6, 30, 15.0, TimeScaleFactory.getTAI)
    okTai = TimeScalesFactory.getTAI()
    okUtc = TimeScalesFactory.getUTC()
    okA = new AbsoluteDate(2020, 3, 1, 12, 0, 0.0, okTai)
    okB = new AbsoluteDate(2021, 6, 15, 6, 30, 15.0, okTai)

  // Construction from calendar date.
  @Benchmark def nf_construct: AbsoluteTime =
    new AbsoluteTime(2020, 3, 1, 12, 0, 0.0, TimeScaleFactory.getTAI)
  @Benchmark def ok_construct: AbsoluteDate =
    new AbsoluteDate(2020, 3, 1, 12, 0, 0.0, okTai)

  // Duration between two epochs.
  @Benchmark def nf_durationFrom: Double = nfA.durationFrom(nfB).toDouble
  @Benchmark def ok_durationFrom: Double = okA.durationFrom(okB)

  // Time shift.
  @Benchmark def nf_shift: AbsoluteTime = nfA.++(86400.0)
  @Benchmark def ok_shift: AbsoluteDate = okA.shiftedBy(86400.0)

  // UTC<->TAI offset (leap-second path).
  @Benchmark def nf_utcOffset: Double = nfA.offsetBetween(TimeScaleFactory.getUTC, TimeScaleFactory.getTAI).toDouble
  @Benchmark def ok_utcOffset: Double = okUtc.offsetFromTAI(okA)
