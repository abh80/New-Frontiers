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

  "getAngularMomentum" should "compute specific angular momentum as position cross velocity" in {
    val pos = new Vector3D(1, -2, 3)
    val vel = new Vector3D(-9, 8, -7)

    // General case: r × v = (-10, -20, -10)
    new KinematicState(pos, vel).getAngularMomentum shouldEqual Vector3D(-10, -20, -10)

    // Confirm non-zero acceleration is ignored
    new KinematicState(pos, vel, new Vector3D(1, 2, 3)).getAngularMomentum shouldEqual Vector3D(-10, -20, -10)

    // Trigonometric cases
    new KinematicState(Vector3D.PLUS_I, Vector3D.MINUS_I).getAngularMomentum shouldEqual Vector3D.Zero // r × (-r) = 0
    new KinematicState(Vector3D.PLUS_I, Vector3D.PLUS_J).getAngularMomentum shouldEqual Vector3D.PLUS_K // i × j = k

    // Edge case: zero position
    new KinematicState(Vector3D.Zero, vel).getAngularMomentum shouldEqual Vector3D.Zero // 0 × v = 0
  }

  "shiftBy" should "update position and velocity using constant-acceleration kinematics" in {
    val p1 = new Vector3D(1.0, 0.1, 10.0)
    val p2 = new Vector3D(2.0, 0.2, 20.0)
    val v = new Vector3D(-1.0, -0.1, -10.0)
    val a = new Vector3D(0.0, 0.2, 0.0)

    // Test with zero acceleration, negative dt
    checkResult(new KinematicState(p2, v, Vector3D.Zero), new KinematicState(p1, v, Vector3D.Zero).shiftBy(-1.0))

    // Test with zero acceleration, positive dt
    checkResult(new KinematicState(p1, v, Vector3D.Zero), new KinematicState(p2, v, Vector3D.Zero).shiftBy(1.0))

    // Test with non-zero acceleration
    val dt = 2.0
    val expectedPos = p1 + (v * dt) + (a * (0.5 * dt * dt)) // r + v*dt + (1/2)*a*dt^2
    val expectedVel = v + (a * dt) // v + a*dt
    checkResult(
      new KinematicState(expectedPos, expectedVel, a),
      new KinematicState(p1, v, a).shiftBy(dt)
    )

    // Test dt = 0
    checkResult(new KinematicState(p1, v, a), new KinematicState(p1, v, a).shiftBy(0.0))

    // Test velocityBetween separately
    KinematicState.velocityBetween(p1, p2, -1.0) shouldBe v // (p2 - p1) / (-1.0) = v
  }

  "getAngularVelocity" should "compute angular velocity as position cross velocity over position magnitude squared" in {
    // General case: arbitrary position and velocity
    val pos = new Vector3D(1.0, -2.0, 3.0)
    val vel = new Vector3D(-4.0, 5.0, -6.0)
    // Compute expected: ω = (r × v) / ||r||^2
    // r × v = (1, -2, 3) × (-4, 5, -6) = [(-2)(-6) - (3)(5), (3)(-4) - (1)(-6), (1)(5) - (-2)(-4)] = (12 - 15, -12 + 6, 5 - 8) = (-3, -6, -3)
    // ||r||^2 = 1^2 + (-2)^2 + 3^2 = 1 + 4 + 9 = 14
    // ω = (-3, -6, -3) / 14 = (-3/14, -6/14, -3/14)
    val expected = new Vector3D(-3.0 / 14.0, -6.0 / 14.0, -3.0 / 14.0)
    checkVector(new KinematicState(pos, vel, Vector3D.Zero).getAngularVelocity, expected)

    // Trigonometric case: perpendicular vectors (i × j = k)
    // r = (1, 0, 0), v = (0, 1, 0) => r × v = (0, 0, 1), ||r||^2 = 1 => ω = (0, 0, 1)
    checkVector(
      new KinematicState(Vector3D.PLUS_I, Vector3D.PLUS_J).getAngularVelocity,
      Vector3D.PLUS_K
    )

    // Trigonometric case: parallel vectors (i × i = 0)
    // r = (1, 0, 0), v = (1, 0, 0) => r × v = (0, 0, 0), ||r||^2 = 1 => ω = (0, 0, 0)
    checkVector(
      new KinematicState(Vector3D.PLUS_I, Vector3D.PLUS_I).getAngularVelocity,
      Vector3D.Zero
    )

    // Edge case: zero position
    // r = (0, 0, 0), v = arbitrary => ||r||^2 = 0 => ω = (0, 0, 0)
    checkVector(
      new KinematicState(Vector3D.Zero, vel).getAngularVelocity,
      Vector3D.Zero
    )

    // Edge case: small position vector to test numerical stability
    // r = (1e-10, 0, 0), v = (0, 1e-10, 0) => r × v = (0, 0, 1e-20), ||r||^2 = 1e-20 => ω = (0, 0, 1)
    checkVector(
      new KinematicState(new Vector3D(1e-10, 0.0, 0.0), new Vector3D(0.0, 1e-10, 0.0)).getAngularVelocity,
      Vector3D.PLUS_K
    )
  }

  private def checkResult(expected: KinematicState, actual: KinematicState, tolerance: Double = 1.0e-15) = {
    expected.position.x shouldEqual actual.position.x +- tolerance
    expected.position.y shouldEqual actual.position.y +- tolerance
    expected.position.z shouldEqual actual.position.z +- tolerance
    expected.velocity.x shouldEqual actual.velocity.x +- tolerance
    expected.velocity.y shouldEqual actual.velocity.y +- tolerance
    expected.velocity.z shouldEqual actual.velocity.z +- tolerance
  }

  private def checkVector(actual: Vector3D, expected: Vector3D, tolerance: Double = 1.0e-15): Unit = {
    val diff = actual - expected
    val error = diff.magnitude // Euclidean norm of difference
    assert(error < tolerance, s"Vector mismatch: $actual != $expected, error = $error")
  }
}
