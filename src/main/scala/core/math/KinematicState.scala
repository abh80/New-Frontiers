package org.abh80.nf
package core.math

import util.TimeShiftable

/**
 * Represents the state of an object in terms of its kinematic properties (in space-time): position, velocity, and acceleration.
 * Provides methods to calculate its state after a time shift, as well as methods to compute derived values such as
 * position and velocity shifts. All vectors are in SI units (meters for position, meters/second for velocity,
 * meters/second² for acceleration).
 *
 * Instances of this class are immutable and time-shiftable, adhering to the [[TimeShiftable]] trait.
 *
 * @see [[TimeShiftable]]
 * @param position     The position vector of the object in 3D space (meters).
 * @param velocity     The velocity vector of the object in 3D space (meters/second).
 * @param acceleration The acceleration vector of the object in 3D space (meters/second²).
 */
case class KinematicState(position: Vector3D, velocity: Vector3D, acceleration: Vector3D) extends TimeShiftable[KinematicState] {

  /**
   * Auxiliary constructor for creating a KinematicState with the specified position.
   * Sets velocity and acceleration to zero by default.
   *
   * @param position The initial position vector of the kinematic state (meters).
   */
  def this(position: Vector3D) = this(position, Vector3D.Zero, Vector3D.Zero)

  /**
   * Auxiliary constructor for creating a KinematicState with the specified position and velocity.
   * Sets acceleration to zero by default.
   *
   * @param position The initial position vector of the kinematic state (meters).
   * @param velocity The initial velocity vector of the kinematic state (meters/second).
   */
  def this(position: Vector3D, velocity: Vector3D) = this(position, velocity, Vector3D.Zero)

  /**
   * Shifts the kinematic state forward by a time interval dt using constant-acceleration kinematics.
   * Returns a new KinematicState with updated position and velocity, while acceleration remains unchanged.
   *
   * @param dt Time interval in seconds.
   * @return A new [[KinematicState]] representing the state after time dt.
   */
  override def ++(dt: Double): KinematicState =
    KinematicState(
      positionShiftedBy(dt),
      velocityShiftedBy(dt),
      acceleration
    )

  /**
   * Calculates the position after a time interval dt using the kinematic equation:
   *   r(t + dt) = r(t) + v(t)·dt + (1/2)·a(t)·dt²
   *
   * where:
   *   - r(t) is the initial position (meters)
   *   - v(t) is the initial velocity (meters/second)
   *   - a(t) is the acceleration (meters/second²)
   *   - dt is the time interval (seconds)
   *
   * @param dt Time interval in seconds.
   * @return New position vector after time dt (meters).
   */
  def positionShiftedBy(dt: Double): Vector3D =
    this.position + (this.velocity * dt) + (this.acceleration * (0.5 * dt * dt))

  /**
   * Calculates the velocity after a time interval dt using the kinematic equation:
   *   v(t + dt) = v(t) + a(t)·dt
   *
   * where:
   *   - v(t) is the initial velocity (meters/second)
   *   - a(t) is the acceleration (meters/second²)
   *   - dt is the time interval (seconds)
   *
   * @param dt Time interval in seconds.
   * @return New velocity vector after time dt (meters/second).
   */
  def velocityShiftedBy(dt: Double): Vector3D =
    this.velocity + (this.acceleration * dt)

  /**
   * Returns a new [[KinematicState]] with all components negated.
   * Negation applies to position, velocity, and acceleration vectors.
   *
   * @return A new [[KinematicState]] with negated position, velocity, and acceleration.
   */
  def negate: KinematicState = KinematicState(position.negate, velocity.negate, acceleration.negate)

  /**
   * Normalizes the position vector and adjusts velocity and acceleration to maintain consistent derivatives.
   *
   * The position is normalized to unit length: u = r / |r|, where r is the position vector.
   * The velocity is computed as the derivative of the normalized position:
   *   u̇ = v - (u · v)·u, where v is the original velocity.
   * The acceleration is computed as the derivative of u̇:
   *   ü = w - 2(u · v)·v + (3(u · v)² - v · v - u · w)·u, where w is the original acceleration.
   *
   * @return A new [[KinematicState]] with normalized position and consistent velocity and acceleration derivatives.
   */
  def normalize: KinematicState = {
    val r = position.magnitude
    if (r == 0.0) throw new ArithmeticException("Cannot normalize zero norm vector")
    val inv = 1.0 / r
    val u = position * inv
    val q = velocity * inv
    val s = acceleration * inv
    val alpha = u dot q
    val gamma = q dot q
    val beta = u dot s
    val uDot = q - (u * alpha)
    val uDDot = s - (q * (2.0 * alpha)) + (u * (3.0 * alpha * alpha - gamma - beta))
    KinematicState(u, uDot, uDDot)
  }

  /**
   * Computes the angular momentum of the object based on its position and velocity.
   *
   * The angular momentum L is calculated as:
   *   L = r × v
   * where:
   *   - r is the position vector (meters)
   *   - v is the velocity vector (meters/second)
   *
   * The resulting vector is perpendicular to both the position and velocity vectors,
   * with magnitude equal to |r|·|v|·sin(θ), where θ is the angle between r and v.
   *
   * @note This is the momentum for a unit mass. Multiply this result by the object's mass to get the actual momentum.
   * @return A [[Vector3D]] representing the angular momentum vector.
   */
  def getAngularMomentum: Vector3D = position X velocity

  /**
   * Computes the angular velocity of the object based on its position and velocity.
   *
   * The angular velocity ω is calculated as:
   *   ω = (r × v) / |r|²
   * where:
   *   - r is the position vector (meters)
   *   - v is the velocity vector (meters/second)
   *   - |r|² is the squared magnitude of the position vector
   *
   * If the position vector is zero (|r| = 0), the angular velocity is undefined,
   * and a zero vector is returned to avoid division by zero.
   *
   * @return A [[Vector3D]] representing the angular velocity in radians per second.
   */
  def getAngularVelocity: Vector3D = {
    val m = position.magnitude
    val rSquared = m * m
    if (rSquared == 0.0) {
      Vector3D.Zero // Return zero vector if position is at origin
    } else {
      getAngularMomentum * (1.0 / rSquared) // (r × v) / ||r||^2
    }
  }

  /**
   * Returns a string representation of the kinematic state.
   * Format: {Pos[position], Vel[velocity], Acc[acceleration]}.
   *
   * @return A string describing the position, velocity, and acceleration vectors.
   */
  override def toString: String = f"{Pos[${position.toString}], Vel[${velocity.toString}], Acc[${acceleration.toString}]}"
}

object KinematicState {
  /**
   * A constant representing a zero kinematic state, with zero position, velocity, and acceleration.
   */
  val ZERO = new KinematicState(Vector3D.Zero, Vector3D.Zero, Vector3D.Zero)

  def velocityBetween(start: Vector3D, end: Vector3D, dt: Double): Vector3D =
    (end - start) / dt

  /**
   * Provides binary operations for [[KinematicState]] instances.
   */
  implicit class BinOp(self: KinematicState) {
    /**
     * Scales the kinematic state by a scalar value.
     * Each component (position, velocity, acceleration) is multiplied by the scalar.
     *
     * @param scalar The scaling factor.
     * @return A new [[KinematicState]] with scaled components.
     */
    def *(scalar: Double) =
      new KinematicState(
        self.position * scalar,
        self.velocity * scalar,
        self.acceleration * scalar
      )

    /**
     * Subtracts another kinematic state from this one.
     * Performs component-wise subtraction: position - position, velocity - velocity, acceleration - acceleration.
     *
     * @param second The kinematic state to subtract.
     * @return A new [[KinematicState]] representing the difference.
     */
    def -(second: KinematicState) =
      new KinematicState(
        self.position - second.position,
        self.velocity - second.velocity,
        self.acceleration - second.acceleration
      )

    /**
     * Computes the cross product of two kinematic states.
     * The cross product is applied component-wise, with velocity and acceleration terms derived to maintain consistency:
     * - Position: P₁ × P₂
     * - Velocity: (V₁ × P₂) + (P₁ × V₂)
     * - Acceleration: (A₁ × P₂) + (P₁ × A₂) + 2(V₁ × V₂)
     *
     * where P₁, V₁, A₁ are the position, velocity, and acceleration of the first state,
     * and P₂, V₂, A₂ are those of the second state.
     *
     * @param second The kinematic state to compute the cross product with.
     * @return A new [[KinematicState]] representing the cross product.
     */
    def X(second: KinematicState): KinematicState = {
      // Correct velocity term: (V₁ × P₂) + (P₁ × V₂)
      val vel = (self.velocity X second.position) + (self.position X second.velocity)

      // Correct acceleration term: (A₁ × P₂) + (P₁ × A₂) + 2(V₁ × V₂)
      val acc = (self.acceleration X second.position) +
        (self.position X second.acceleration) +
        ((self.velocity X second.velocity) * 2.0)

      new KinematicState(
        self.position X second.position,
        vel,
        acc
      )
    }
  }
}