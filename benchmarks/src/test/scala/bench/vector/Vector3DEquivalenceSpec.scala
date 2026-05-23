package bench.vector

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalactic.Tolerance.*

import org.abh80.nf.core.math.Vector3D
import org.abh80.nf.core.metrics.AngleUnit
import org.hipparchus.geometry.euclidean.threed.{Vector3D as HVector3D, Rotation, RotationConvention}

// ScalaTest Matchers reserves `a`/`an` as DSL words, so the vectors are `va`/`vb`, not `a`/`b`.
class Vector3DEquivalenceSpec extends AnyFlatSpec with Matchers:
  private val va = Vector3D(1.0, 2.0, 3.0)
  private val vb = Vector3D(-4.0, 5.0, 6.0)
  private val ha = new HVector3D(1.0, 2.0, 3.0)
  private val hb = new HVector3D(-4.0, 5.0, 6.0)
  private val tol = 1e-12

  "NF Vector3D" should "match Hipparchus on add" in {
    val r = va + vb; val h = ha.add(hb)
    r.x shouldBe (h.getX +- tol); r.y shouldBe (h.getY +- tol); r.z shouldBe (h.getZ +- tol)
  }
  it should "match Hipparchus on dot" in { (va dot vb) shouldBe (ha.dotProduct(hb) +- tol) }
  it should "match Hipparchus on cross" in {
    val r = va X vb; val h = ha.crossProduct(hb)
    r.x shouldBe (h.getX +- tol); r.y shouldBe (h.getY +- tol); r.z shouldBe (h.getZ +- tol)
  }
  it should "match Hipparchus on magnitude" in { va.magnitude shouldBe (ha.getNorm +- tol) }
  it should "match Hipparchus on normalize" in {
    val r = va.normalize; val h = ha.normalize
    r.x shouldBe (h.getX +- tol); r.y shouldBe (h.getY +- tol); r.z shouldBe (h.getZ +- tol)
  }
  it should "match Hipparchus on angleTo" in {
    va.angleTo(vb)().toRadians shouldBe (HVector3D.angle(ha, hb) +- tol)
  }
  it should "match Hipparchus on rotateX (active, VECTOR_OPERATOR)" in {
    val theta = 0.7
    val r = va.rotateX(AngleUnit.Radian(theta))
    val h = new Rotation(new HVector3D(1, 0, 0), theta, RotationConvention.VECTOR_OPERATOR).applyTo(ha)
    r.x shouldBe (h.getX +- tol); r.y shouldBe (h.getY +- tol); r.z shouldBe (h.getZ +- tol)
  }
