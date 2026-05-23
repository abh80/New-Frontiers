package bench.vector

import org.openjdk.jmh.annotations.*
import java.util.concurrent.TimeUnit

import org.abh80.nf.core.math.Vector3D
import org.abh80.nf.core.metrics.AngleUnit
import org.hipparchus.geometry.euclidean.threed.{Vector3D as HVector3D, Rotation, RotationConvention}

@State(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@Fork(1)
@Warmup(iterations = 5, time = 1)
@Measurement(iterations = 5, time = 1)
class Vector3DBenchmarks:
  // Initialised in declaration to dummy values, set for real in @Setup.
  var a: Vector3D = Vector3D.Zero
  var b: Vector3D = Vector3D.Zero
  var ha: HVector3D = HVector3D.ZERO
  var hb: HVector3D = HVector3D.ZERO
  val theta = 0.7
  val nfAngle = AngleUnit.Radian(0.7)

  @Setup(Level.Trial)
  def setup(): Unit =
    a = Vector3D(1.0, 2.0, 3.0)
    b = Vector3D(-4.0, 5.0, 6.0)
    ha = new HVector3D(1.0, 2.0, 3.0)
    hb = new HVector3D(-4.0, 5.0, 6.0)

  @Benchmark def nf_add: Vector3D     = a + b
  @Benchmark def ok_add: HVector3D    = ha.add(hb)
  @Benchmark def nf_sub: Vector3D     = a - b
  @Benchmark def ok_sub: HVector3D    = ha.subtract(hb)
  @Benchmark def nf_scale: Vector3D   = a * 3.0
  @Benchmark def ok_scale: HVector3D  = ha.scalarMultiply(3.0)
  @Benchmark def nf_dot: Double       = a dot b
  @Benchmark def ok_dot: Double       = ha.dotProduct(hb)
  @Benchmark def nf_cross: Vector3D   = a X b
  @Benchmark def ok_cross: HVector3D  = ha.crossProduct(hb)
  @Benchmark def nf_normalize: Vector3D  = a.normalize
  @Benchmark def ok_normalize: HVector3D = ha.normalize
  @Benchmark def nf_magnitude: Double = a.magnitude
  @Benchmark def ok_magnitude: Double = ha.getNorm
  @Benchmark def nf_angleTo: Double   = a.angleTo(b)().toRadians
  @Benchmark def ok_angleTo: Double   = HVector3D.angle(ha, hb)
  @Benchmark def nf_rotateX: Vector3D = a.rotateX(nfAngle)
  @Benchmark def ok_rotateX: HVector3D =
    new Rotation(new HVector3D(1, 0, 0), theta, RotationConvention.VECTOR_OPERATOR).applyTo(ha)
