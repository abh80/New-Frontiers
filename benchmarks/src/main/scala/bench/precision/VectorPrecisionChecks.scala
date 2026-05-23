package bench.precision

import bench.support.PrecisionResult
import org.abh80.nf.core.math.Vector3D
import org.abh80.nf.core.metrics.AngleUnit
import org.hipparchus.geometry.euclidean.threed.{Vector3D as HVector3D}

/** Precision via analytic identities:
 *   - normalize: |magnitude(normalize(v)) - 1|
 *   - cross: |(i x j) - k| component distance
 *   - rotate: |rotateX by 2*pi - identity| (round trip)  */
object VectorPrecisionChecks:
  private val cat = "Vector3D"

  def run(): Seq[PrecisionResult] =
    val v  = Vector3D(1.0, 2.0, 3.0)
    val hv = new HVector3D(1.0, 2.0, 3.0)

    val nfNorm = math.abs(v.normalize.magnitude - 1.0)
    val okNorm = math.abs(hv.normalize.getNorm - 1.0)

    val nfCross =
      val c = Vector3D.PLUS_I X Vector3D.PLUS_J
      dist(c.x, c.y, c.z, 0, 0, 1)
    val okCross =
      val c = HVector3D.PLUS_I.crossProduct(HVector3D.PLUS_J)
      dist(c.getX, c.getY, c.getZ, 0, 0, 1)

    val nfRot =
      val r = v.rotateX(AngleUnit.Radian(2 * math.Pi))
      dist(r.x, r.y, r.z, v.x, v.y, v.z)
    val okRot =
      val r = new org.hipparchus.geometry.euclidean.threed.Rotation(
        new HVector3D(1, 0, 0), 2 * math.Pi,
        org.hipparchus.geometry.euclidean.threed.RotationConvention.VECTOR_OPERATOR).applyTo(hv)
      dist(r.getX, r.getY, r.getZ, hv.getX, hv.getY, hv.getZ)

    Seq(
      PrecisionResult(cat, "normalize", "nf", nfNorm),
      PrecisionResult(cat, "normalize", "ok", okNorm),
      PrecisionResult(cat, "cross",     "nf", nfCross),
      PrecisionResult(cat, "cross",     "ok", okCross),
      PrecisionResult(cat, "rotateX",   "nf", nfRot),
      PrecisionResult(cat, "rotateX",   "ok", okRot)
    )

  private def dist(x: Double, y: Double, z: Double, ex: Double, ey: Double, ez: Double): Double =
    math.sqrt((x - ex) * (x - ex) + (y - ey) * (y - ey) + (z - ez) * (z - ez))
