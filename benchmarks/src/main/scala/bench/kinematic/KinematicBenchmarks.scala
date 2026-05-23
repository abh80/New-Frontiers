package bench.kinematic

import org.openjdk.jmh.annotations.*
import java.util.concurrent.TimeUnit

import org.abh80.nf.core.math.{KinematicState, Vector3D}
import org.hipparchus.geometry.euclidean.threed.{Vector3D as HVector3D}
import org.orekit.utils.PVCoordinates

@State(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@Fork(1)
@Warmup(iterations = 5, time = 1)
@Measurement(iterations = 5, time = 1)
class KinematicBenchmarks:
  var ks: KinematicState = KinematicState.ZERO
  var pv: PVCoordinates = PVCoordinates.ZERO

  @Setup(Level.Trial)
  def setup(): Unit =
    ks = KinematicState(Vector3D(7000.0, 0.0, 0.0), Vector3D(0.0, 7.5, 1.0), Vector3D(0.01, 0.0, -0.02))
    pv = new PVCoordinates(
      new HVector3D(7000.0, 0.0, 0.0), new HVector3D(0.0, 7.5, 1.0), new HVector3D(0.01, 0.0, -0.02))

  @Benchmark def nf_shift: KinematicState = ks ++ 120.0
  @Benchmark def ok_shift: PVCoordinates  = pv.shiftedBy(120.0)
  @Benchmark def nf_momentum: Vector3D    = ks.getAngularMomentum
  @Benchmark def ok_momentum: HVector3D   = pv.getMomentum
