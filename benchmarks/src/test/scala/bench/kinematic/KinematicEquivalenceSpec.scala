package bench.kinematic

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalactic.Tolerance.*

import org.abh80.nf.core.math.{KinematicState, Vector3D}
import org.hipparchus.geometry.euclidean.threed.{Vector3D as HVector3D}
import org.orekit.utils.PVCoordinates

class KinematicEquivalenceSpec extends AnyFlatSpec with Matchers:
  private val ks = KinematicState(Vector3D(7000.0, 0.0, 0.0), Vector3D(0.0, 7.5, 1.0), Vector3D(0.01, 0.0, -0.02))
  private val pv = new PVCoordinates(
    new HVector3D(7000.0, 0.0, 0.0), new HVector3D(0.0, 7.5, 1.0), new HVector3D(0.01, 0.0, -0.02))
  private val tol = 1e-9

  "NF KinematicState" should "match Orekit PVCoordinates on shiftedBy position" in {
    val r = (ks ++ 120.0).position
    val h = pv.shiftedBy(120.0).getPosition
    r.x shouldBe (h.getX +- tol); r.y shouldBe (h.getY +- tol); r.z shouldBe (h.getZ +- tol)
  }
  it should "match Orekit PVCoordinates on shiftedBy velocity" in {
    val r = (ks ++ 120.0).velocity
    val h = pv.shiftedBy(120.0).getVelocity
    r.x shouldBe (h.getX +- tol); r.y shouldBe (h.getY +- tol); r.z shouldBe (h.getZ +- tol)
  }
  it should "match Orekit on angular momentum (r x v)" in {
    val r = ks.getAngularMomentum
    val h = pv.getMomentum
    r.x shouldBe (h.getX +- tol); r.y shouldBe (h.getY +- tol); r.z shouldBe (h.getZ +- tol)
  }
