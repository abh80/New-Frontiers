package bench.precision

import bench.support.PrecisionResult
import org.abh80.nf.core.math.{KinematicState, Vector3D}
import org.hipparchus.geometry.euclidean.threed.{Vector3D as HVector3D}
import org.orekit.utils.PVCoordinates

/** Truth: r(t) = r0 + v0 t + 0.5 a t^2 (exact for constant acceleration). */
object KinematicPrecisionChecks:
  // Must match the timing category derived from KinematicBenchmarks (stripSuffix "Benchmarks").
  private val cat = "Kinematic"

  def run(): Seq[PrecisionResult] =
    val p0 = (7000.0, 0.0, 0.0); val v0 = (0.0, 7.5, 1.0); val a0 = (0.01, 0.0, -0.02)
    val t = 120.0
    val ex = p0._1 + v0._1 * t + 0.5 * a0._1 * t * t
    val ey = p0._2 + v0._2 * t + 0.5 * a0._2 * t * t
    val ez = p0._3 + v0._3 * t + 0.5 * a0._3 * t * t

    val ks = KinematicState(Vector3D(p0._1, p0._2, p0._3), Vector3D(v0._1, v0._2, v0._3), Vector3D(a0._1, a0._2, a0._3))
    val nf = (ks ++ t).position
    val nfErr = dist(nf.x, nf.y, nf.z, ex, ey, ez)

    val pv = new PVCoordinates(
      new HVector3D(p0._1, p0._2, p0._3), new HVector3D(v0._1, v0._2, v0._3), new HVector3D(a0._1, a0._2, a0._3))
    val ok = pv.shiftedBy(t).getPosition
    val okErr = dist(ok.getX, ok.getY, ok.getZ, ex, ey, ez)

    Seq(
      PrecisionResult(cat, "propagate_120s", "nf", nfErr),
      PrecisionResult(cat, "propagate_120s", "ok", okErr)
    )

  private def dist(x: Double, y: Double, z: Double, ex: Double, ey: Double, ez: Double): Double =
    math.sqrt((x - ex) * (x - ex) + (y - ey) * (y - ey) + (z - ez) * (z - ez))
