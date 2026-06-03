package org.abh80.nf.java

import org.abh80.nf.core.math.{KinematicState, Vector3D}

/**
 * Java-idiomatic facade for [[org.abh80.nf.core.math.KinematicState]].
 *
 * Provides factory helpers and renames the `++`, `*`, `-`, `X` operators to method names that
 * survive Java's identifier rules.
 */
object Kinematics {

  /** Build a state from position only (velocity, acceleration = 0). */
  def fromPosition(position: Vector3D): KinematicState = new KinematicState(position)

  /** Build a state from position + velocity (acceleration = 0). */
  def fromPositionVelocity(position: Vector3D, velocity: Vector3D): KinematicState =
    new KinematicState(position, velocity)

  /** Build a state from position + velocity + acceleration. */
  def of(position: Vector3D, velocity: Vector3D, acceleration: Vector3D): KinematicState =
    KinematicState(position, velocity, acceleration)

  /** Constant-acceleration state propagation by `dt` seconds. */
  def shiftedBy(state: KinematicState, dt: Double): KinematicState = state ++ dt

  /** Component-wise scaling. */
  def scale(state: KinematicState, factor: Double): KinematicState = state * factor

  /** Component-wise subtraction. */
  def subtract(a: KinematicState, b: KinematicState): KinematicState = a - b

  /** Cross product across both kinematic states (see Scala-side `X`). */
  def cross(a: KinematicState, b: KinematicState): KinematicState = a X b

  /** All-zero state. */
  def zero: KinematicState = KinematicState.ZERO

  /** Specific angular momentum `r × v`. */
  def angularMomentum(state: KinematicState): Vector3D = state.getAngularMomentum

  /** Angular velocity `(r × v) / |r|²`. */
  def angularVelocity(state: KinematicState): Vector3D = state.getAngularVelocity
}
