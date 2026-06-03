package org.abh80.nf.java

import org.abh80.nf.core.math.Vector3D

/**
 * Java-idiomatic facade for [[org.abh80.nf.core.math.Vector3D]] arithmetic.
 *
 * The Scala API uses operators such as `+`, `-`, `*`, `X`, `dot`, and `~`. From Java these
 * surface as mangled JVM names (`$plus`, `$X`, `$tilde`); this object exposes them under
 * readable names so Java callers can write `Vectors.cross(a, b)` instead.
 */
object Vectors {

  /** Sum of two vectors. */
  def add(a: Vector3D, b: Vector3D): Vector3D = a + b

  /** Difference `a - b`. */
  def subtract(a: Vector3D, b: Vector3D): Vector3D = a - b

  /** Vector scaled by a dimensionless factor. */
  def scale(v: Vector3D, factor: Double): Vector3D = v * factor

  /** Vector divided by a scalar. */
  def divide(v: Vector3D, scalar: Double): Vector3D = v / scalar

  /** Dot product `a · b`. */
  def dot(a: Vector3D, b: Vector3D): Double = a dot b

  /** Cross product `a × b`. */
  def cross(a: Vector3D, b: Vector3D): Vector3D = a X b

  /**
   * Component-wise approximate equality within a tolerance.
   * Mirrors the Scala `~` operator.
   */
  def approxEquals(a: Vector3D, b: Vector3D, epsilon: Double): Boolean =
    a.~(b, epsilon)

  /** Approximate equality with the library default tolerance (1e-10). */
  def approxEquals(a: Vector3D, b: Vector3D): Boolean = a.~(b)

  /** Vector with all components negated. */
  def negate(v: Vector3D): Vector3D = v.negate

  /** Normalized (unit length) vector — returns the original when length is zero/NaN. */
  def normalize(v: Vector3D): Vector3D = v.normalize

  /** Euclidean magnitude of the vector. */
  def magnitude(v: Vector3D): Double = v.magnitude

  /** Origin (0, 0, 0). */
  def zero: Vector3D = Vector3D.Zero

  /** Construct a Vector3D from x, y, z meters. */
  def of(x: Double, y: Double, z: Double): Vector3D = Vector3D(x, y, z)
}
