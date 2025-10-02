package org.abh80.nf
package core.math

import core.metrics.{AngleUnit, DistanceUnit}

/**
 * Represents a 3-dimensional vector with x, y, and z components in meters.
 * Provides common vector operations and geometric transformations.
 * All operations are immutable and return new instances.
 *
 * @param x The x-component (in meters).
 * @param y The y-component (in meters).
 * @param z The z-component (in meters).
 */
final case class Vector3D(x: Double, y: Double, z: Double) {
  /**
   * Calculates the Euclidean distance to another vector.
   * Formula: sqrt((x2 - x1)^2 + (y2 - y1)^2 + (z2 - z1)^2)
   *
   * @param second The other vector.
   * @param factory Factory function to create a DistanceUnit from a Double.
   * @tparam T The type of DistanceUnit.
   * @return The distance as a DistanceUnit.
   */
  def distanceTo[T <: DistanceUnit](second: Vector3D)(factory: Double => T = DistanceUnit.Meter.apply): T = {
    val dx = second.x - x
    val dy = second.y - y
    val dz = second.z - z
    val distanceInMeters = Math.sqrt(dx * dx + dy * dy + dz * dz)
    if (distanceInMeters.isNaN || distanceInMeters.isInfinite) {
      factory(0.0) // Handle invalid inputs gracefully
    } else {
      factory(distanceInMeters)
    }
  }

  /**
   * Returns the normalized (unit length) vector.
   * If magnitude is zero, returns the original vector.
   * @return The normalized vector.
   */
  def normalize: Vector3D = {
    val mag = magnitude
    if (mag == 0 || mag.isNaN || mag.isInfinite) this else this / mag
  }

  /**
   * Computes the Euclidean magnitude (length) of the vector in meters.
   * Formula: sqrt(x^2 + y^2 + z^2)
   * @return The magnitude in meters, or 0.0 if NaN or infinite.
   */
  def magnitude: Double = {
    val mag = Math.sqrt(x * x + y * y + z * z)
    if (mag.isNaN || mag.isInfinite) 0.0 else mag
  }

  /**
   * Calculates the angle between this vector and another.
   * Formula: acos(dotProduct / (magnitude1 * magnitude2))
   * @param second The other vector.
   * @param factory Factory function to create an AngleUnit from a Double.
   * @tparam T The type of AngleUnit.
   * @return The angle as an AngleUnit.
   */
  def angleTo[T <: AngleUnit](second: Vector3D)(factory: Double => T = AngleUnit.Radian.apply): T = {
    val dotProduct = this.dot(second)
    val magnitudes = this.magnitude * second.magnitude
    AngleUnit.fromRadians(Math.acos(dotProduct / magnitudes), factory)
  }

  /**
   * Rotates the vector around the X axis by the given angle.
   * @param angle The angle to rotate by.
   * @return The rotated vector.
   */
  def rotateX(angle: AngleUnit): Vector3D = {
    val cos = Math.cos(angle.toRadians)
    val sin = Math.sin(angle.toRadians)
    Vector3D(
      x,
      y * cos - z * sin,
      y * sin + z * cos
    )
  }

  /**
   * Rotates the vector around the Y axis by the given angle.
   * @param angle The angle to rotate by.
   * @return The rotated vector.
   */
  def rotateY(angle: AngleUnit): Vector3D = {
    val cos = Math.cos(angle.toRadians)
    val sin = Math.sin(angle.toRadians)
    Vector3D(
      x * cos + z * sin,
      y,
      -x * sin + z * cos
    )
  }

  /**
   * Rotates the vector around the Z axis by the given angle.
   * @param angle The angle to rotate by.
   * @return The rotated vector.
   */
  def rotateZ(angle: AngleUnit): Vector3D = {
    val cos = Math.cos(angle.toRadians)
    val sin = Math.sin(angle.toRadians)
    Vector3D(
      x * cos - y * sin,
      x * sin + y * cos,
      z
    )
  }

  /**
   * Returns a string representation of the vector.
   * @return String in the format Vector3D(x, y, z)
   */
  override def toString: String = s"Vector3D($x, $y, $z)"

  /**
   * Returns a vector with all components negated.
   * @return The negated vector.
   */
  def negate: Vector3D = Vector3D(-x, -y, -z)
}

/**
 * Companion object for Vector3D.
 * Provides utility methods and implicit operations for vectors.
 */
object Vector3D {
  /**
   * The zero vector (0, 0, 0).
   */
  val Zero: Vector3D = Vector3D(0, 0, 0)

  /**
   * Unit vector in the positive x direction (1, 0, 0).
   */
  val PLUS_I: Vector3D = Vector3D(1, 0, 0)

  /**
   * Unit vector in the negative x direction (-1, 0, 0).
   */
  val MINUS_I: Vector3D = Vector3D(-1, 0, 0)

  /**
   * Unit vector in the positive y direction (0, 1, 0).
   */
  val PLUS_J: Vector3D = Vector3D(0, 1, 0)

  /**
   * Unit vector in the negative y direction (0, -1, 0).
   */
  val MINUS_J: Vector3D = Vector3D(0, -1, 0)

  /**
   * Unit vector in the positive z direction (0, 0, 1).
   */
  val PLUS_K: Vector3D = Vector3D(0, 0, 1)

  /**
   * Unit vector in the negative z direction (0, 0, -1).
   */
  val MINUS_K: Vector3D = Vector3D(0, 0, -1)

  /**
   * Creates a Vector3D from an array of DistanceUnit values.
   * @param arr Array of 3 DistanceUnit elements (x, y, z).
   * @throws IllegalArgumentException if the array does not have exactly 3 elements.
   * @return The corresponding Vector3D.
   */
  def fromArray[T <: DistanceUnit](arr: Array[T]): Vector3D = {
    if arr.length != 3 then throw new IllegalArgumentException(s"Vector array expected to have 3 elements (x,y,z) distances but found ${arr.length} elements.")
    Vector3D(arr(0), arr(1), arr(2))
  }

  /**
   * Creates a Vector3D from three DistanceUnit values.
   * @param x The x-component.
   * @param y The y-component.
   * @param z The z-component.
   * @return The corresponding Vector3D.
   */
  def apply(x: DistanceUnit, y: DistanceUnit, z: DistanceUnit): Vector3D = Vector3D(x.toMeter, y.toMeter, z.toMeter)

  /**
   * Provides arithmetic and geometric operations for Vector3D.
   * @param self The vector instance.
   */
  implicit class BinOp(self: Vector3D) {
    /**
     * Adds two vectors.
     * @param other The other vector.
     * @return The sum vector.
     */
    def +(other: Vector3D): Vector3D = Vector3D(self.x + other.x, self.y + other.y, self.z + other.z)

    /**
     * Subtracts another vector from this vector.
     * @param other The other vector.
     * @return The difference vector.
     */
    def -(other: Vector3D): Vector3D = Vector3D(self.x - other.x, self.y - other.y, self.z - other.z)

    /**
     * Multiplies the vector by a scalar.
     * @param scalar The scalar value.
     * @return The scaled vector.
     */
    def *(scalar: Double): Vector3D = Vector3D(self.x * scalar, self.y * scalar, self.z * scalar)

    /**
     * Checks if two vectors are approximately equal within a given epsilon.
     * @param other The other vector.
     * @param epsilon The tolerance.
     * @return True if all components are within epsilon, false otherwise.
     */
    def ~(other: Vector3D, epsilon: Double = 1e-10): Boolean =
      Math.abs(self.x - other.x) < epsilon &&
        Math.abs(self.y - other.y) < epsilon &&
        Math.abs(self.z - other.z) < epsilon

    /**
     * Computes the dot product of two vectors.
     * @param other The other vector.
     * @return The dot product.
     */
    def dot(other: Vector3D): Double = self.x * other.x + self.y * other.y + self.z * other.z

    /**
     * Computes the cross product of two vectors.
     * @param other The other vector.
     * @return The cross product vector.
     */
    def X(other: Vector3D): Vector3D = Vector3D(
      self.y * other.z - self.z * other.y,
      self.z * other.x - self.x * other.z,
      self.x * other.y - self.y * other.x
    )

    /**
     * Divides the vector by a scalar.
     * @param scalar The scalar value.
     * @return The scaled vector.
     */
    def /(scalar: Double): Vector3D = Vector3D(self.x / scalar, self.y / scalar, self.z / scalar)
  }
}