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
   * r(t + dt) = r(t) + v(t)dt + (1/2)a(t)dt²
   *
   * where:
   * - r(t) is the initial position (meters)
   * - v(t) is the initial velocity (meters/second)
   * - a(t) is the acceleration (meters/second²)
   * - dt is the time interval (seconds)
   *
   *
   * @param dt Time interval in seconds.
   * @return New position vector after time dt (meters).
   */
  def positionShiftedBy(dt: Double): Vector3D =
    this.position + (this.velocity * dt) + (this.acceleration * (0.5 * dt * dt))

  /**
   * Calculates the velocity after a time interval dt using the kinematic equation:
   * v(t + dt) = v(t) + a(t)dt
   *
   * where:
   * - v(t) is the initial velocity (meters/second)
   * - a(t) is the acceleration (meters/second²)
   * - dt is the time interval (seconds)
   *
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
   * The position is normalized to unit length: u = r / ||r||, where r is the position vector.
   * The velocity is computed as the derivative of the normalized position:
   * u̇ = v - (u · v)u, where v is the original velocity.
   * The acceleration is computed as the derivative of u̇:
   * ü = w - 2(u · v)v + (3(u · v)² - v · v - u · w)u, where w is the original acceleration.
   *
   * @return A new [[KinematicState]] with normalized position and consistent velocity and acceleration derivatives.
   */
  def normalize: KinematicState = {
    val inv = 1.0 / position.magnitude
    val u = position * inv // Normalized position
    val v = velocity * inv // Scaled velocity
    val w = acceleration * inv // Scaled acceleration
    val uv = u.dot(v) // Dot product u · v
    val v2 = v.dot(v) // Dot product v · v
    val uw = u.dot(w) // Dot product u · w
    val uDot = v - (u * uv) // Velocity: derivative of normalized position
    val uDotDot = w - (v * 2 * uv) + (u * (3 * uv * uv - v2 - uw)) // Acceleration: derivative of uDot
    KinematicState(u, uDot, uDotDot)
  }


  /**
   * Computes the angular momentum of the object based on its position and velocity.
   *
   * The angular momentum \(\mathbf{L}\) is calculated as:
   * \[
   * \mathbf{L} = \mathbf{r} \times \mathbf{v}
   * \]
   * where:
   * - \(\mathbf{r}\) is the position vector (meters)
   * - \(\mathbf{v}\) is the velocity vector (meters/second)
   *
   * The resulting vector is perpendicular to both the position and velocity vectors,
   * with magnitude equal to r·v·sin(θ), where θ is the angle between r and v.
   *
   * @note this is the momentum for a unit mass, you may want to multiply this result with the `mass` of the object to get the accurate momentum.
   * @return A [[Vector3D]] representing the angular momentum vector.
   */
  def getMomentum: Vector3D = position X velocity

  /**
   * Computes the angular velocity of the object based on its position and velocity.
   *
   * The angular velocity \(\omega\) is calculated as:
   * \[
   * \mathbf{\omega} = \frac{\mathbf{r} \times \mathbf{v}}{\|\mathbf{r}\|^2}
   * \]
   * where:
   * - \(\mathbf{r}\) is the position vector (meters)
   * - \(\mathbf{v}\) is the velocity vector (meters/second)
   * - \(\|\mathbf{r}\|^2\) is the squared magnitude of the position vector
   *
   * If the position vector is zero (\(\|\mathbf{r}\|=0\)), the angular velocity is undefined,
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
      getMomentum * (1.0 / rSquared) // (r × v) / ||r||^2
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
     * - Position: r₁ × r₂
     * - Velocity: (r₁ × v₂) + (r₂ × v₁)
     * - Acceleration: (r₁ × a₂) + 2(v₁ × v₂) + (a₁ × r₂)
     *
     * where r₁, v₁, a₁ are the position, velocity, and acceleration of the first state,
     * and r₂, v₂, a₂ are those of the second state.
     *
     * @param second The kinematic state to compute the cross product with.
     * @return A new [[KinematicState]] representing the cross product.
     */
    def X(second: KinematicState) =
      new KinematicState(
        self.position X second.position,
        (self.position X second.velocity) + (second.position X self.velocity),
        (self.position X second.acceleration) + ((self.velocity X second.velocity) * 2) + (self.acceleration X second.position)
      )
  }
}