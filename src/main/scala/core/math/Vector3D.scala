package org.abh80.nf
package core.math

import core.metrics.{AngleUnit, DistanceUnit}


final case class Vector3D(x: Double, y: Double, z: Double) {
  def magnitude: Double = Math.sqrt(Math.pow(x, 2) * Math.pow(y, 2) * Math.pow(z, 2))

  def distanceTo[T <: DistanceUnit](second: Vector3D)(factory: Double => T = DistanceUnit.Meter.apply): T = {
    val distanceInMeters = Math.sqrt(
      Math.pow(second.x - x, 2) +
        Math.pow(second.y - y, 2) +
        Math.pow(second.z - z, 2)
    )
    factory(distanceInMeters)
  }

  def normalize: Vector3D = {
    val mag = magnitude
    if (mag == 0) this else this / mag
  }

  def angleTo[T <: AngleUnit](second: Vector3D)(factory: Double => T = AngleUnit.Radian.apply): T = {
    val dotProduct = this.dot(second)
    val magnitudes = this.magnitude * second.magnitude
    AngleUnit.fromRadians(Math.acos(dotProduct / magnitudes), factory)
  }

  override def toString: String = s"Vector3D($x, $y, $z) [m]"
}

object Vector3D {
  val Zero: Vector3D = Vector3D(0, 0, 0)

  def fromArray(arr: Array[DistanceUnit]): Vector3D = {
    if arr.length != 3 then throw new IllegalArgumentException(s"Vector array expected to have 3 elements (x,y,z) distances but found ${arr.length} elements.")

    Vector3D(arr(0), arr(1), arr(2))
  }

  def apply(x: DistanceUnit, y: DistanceUnit, z: DistanceUnit): Vector3D = Vector3D(x.toMeter, y.toMeter, z.toMeter)

  implicit class BinOp(self: Vector3D) {
    def +(other: Vector3D): Vector3D = Vector3D(self.x + other.x, self.y + other.y, self.z + other.z)

    def -(other: Vector3D): Vector3D = Vector3D(self.x - other.x, self.y - other.y, self.z - other.z)

    def *(scalar: Double): Vector3D = Vector3D(self.x * scalar, self.y * scalar, self.z * scalar)

    def ~(other: Vector3D, epsilon: Double = 1e-10): Boolean =
      Math.abs(self.x - other.x) < epsilon &&
        Math.abs(self.y - other.y) < epsilon &&
        Math.abs(self.z - other.z) < epsilon

    def dot(other: Vector3D): Double = self.x * other.x + self.y * other.y + self.z * other.z

    def X(other: Vector3D): Vector3D = Vector3D(
      self.y * other.z - self.z * other.y,
      self.z * other.x - self.x * other.z,
      self.x * other.y - self.y * other.x
    )

    def /(scalar: Double): Vector3D = Vector3D(self.x / scalar, self.y / scalar, self.z / scalar)
  }
}