package org.abh80.nf
package core.math

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class KinematicStateSpec extends AnyFlatSpec with Matchers {
  "KinematicState constructors" should "initialize with correct position, velocity, and acceleration" in {
    val pos = new Vector3D(1.0, 0.1, 10.0)
    val vel = new Vector3D(-1.0, -0.1, -10.0)
    val acc = new Vector3D(0.0, 0.2, 0.0)

    // Test position-only constructor
    val k1 = new KinematicState(pos)
    checkResult(k1, KinematicState(pos, Vector3D.Zero, Vector3D.Zero))

    // Test position and velocity constructor
    val k2 = new KinematicState(pos, vel)
    checkResult(k2, KinematicState(pos, vel, Vector3D.Zero))

    // Test full constructor
    val k3 = new KinematicState(pos, vel, acc)
    checkResult(k3, KinematicState(pos, vel, acc))
  }

  "KinematicState binary operations" should "correctly scale and subtract states" in {
    val k1 = new KinematicState(new Vector3D(1.0, 0.1, 10.0), new Vector3D(-1.0, -0.1, -10.0), Vector3D.Zero)
    val k2 = new KinematicState(new Vector3D(3.0, 0.3, 30.0), new Vector3D(-3.0, -0.3, -30.0), Vector3D.Zero)
    val k3 = new KinematicState(new Vector3D(2.0, 0.2, 20.0), new Vector3D(-2.0, -0.2, -20.0), Vector3D.Zero)

    // Test scaling
    checkResult(k2, k1 * 3.0)
    checkResult(KinematicState.ZERO, k1 * 0.0) // Edge case: scaling by zero

    // Test subtraction
    checkResult(k3, k2 - k1)
    checkResult(KinematicState.ZERO, k1 - k1) // Edge case: subtracting identical states
  }

  "toString" should "format position, velocity, and acceleration in expected string format" in {
    val k1 = new KinematicState(new Vector3D(3.0, 0.3, 30.0), new Vector3D(-3.0, -0.3, -30.0), Vector3D.Zero)
    k1.toString shouldEqual "{Pos[Vector3D(3.0, 0.3, 30.0)], Vel[Vector3D(-3.0, -0.3, -30.0)], Acc[Vector3D(0.0, 0.0, 0.0)]}"

    val k2 = new KinematicState(new Vector3D(1.0, 0.1, 10.0), new Vector3D(-1.0, -0.1, -10.0), new Vector3D(0.0, 0.2, 0.0))
    k2.toString shouldEqual "{Pos[Vector3D(1.0, 0.1, 10.0)], Vel[Vector3D(-1.0, -0.1, -10.0)], Acc[Vector3D(0.0, 0.2, 0.0)]}"

    KinematicState.ZERO.toString shouldEqual "{Pos[Vector3D(0.0, 0.0, 0.0)], Vel[Vector3D(0.0, 0.0, 0.0)], Acc[Vector3D(0.0, 0.0, 0.0)]}"
  }

  "getMomentum" should "compute specific angular momentum as position cross velocity" in {
    val pos = new Vector3D(1, -2, 3)
    val vel = new Vector3D(-9, 8, -7)

    // General case: r × v = (-10, -20, -10)
    new KinematicState(pos, vel).getMomentum shouldEqual Vector3D(-10, -20, -10)

    // Confirm non-zero acceleration is ignored
    new KinematicState(pos, vel, new Vector3D(1, 2, 3)).getMomentum shouldEqual Vector3D(-10, -20, -10)

    // Trigonometric cases
    new KinematicState(Vector3D.PLUS_I, Vector3D.MINUS_I).getMomentum shouldEqual Vector3D.Zero // r × (-r) = 0
    new KinematicState(Vector3D.PLUS_I, Vector3D.PLUS_J).getMomentum shouldEqual Vector3D.PLUS_K // i × j = k

    // Edge case: zero position
    new KinematicState(Vector3D.Zero, vel).getMomentum shouldEqual Vector3D.Zero // 0 × v = 0
  }

  "shiftBy" should "return computed value after given dt time" in {

  }

  private def checkResult(expected: KinematicState, actual: KinematicState, tolerance: Double = 1.0e-15) = {
    expected.position.x shouldEqual actual.position.x +- tolerance
    expected.position.y shouldEqual actual.position.y +- tolerance
    expected.position.z shouldEqual actual.position.z +- tolerance
    expected.velocity.x shouldEqual actual.velocity.x +- tolerance
    expected.velocity.y shouldEqual actual.velocity.y +- tolerance
    expected.velocity.z shouldEqual actual.velocity.z +- tolerance
  }
}
