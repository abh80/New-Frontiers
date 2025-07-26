package org.abh80.nf.core.math

import org.scalatest.funsuite.AnyFunSuite
import org.abh80.nf.core.metrics.{DistanceUnit, AngleUnit}

class Vector3DSpec extends AnyFunSuite {
  import Vector3D._

  test("Vector3D magnitude") {
    val v = Vector3D(3, 4, 12)
    assert(v.magnitude === 13)
  }

  test("Vector3D distanceTo") {
    val v1 = Vector3D(0, 0, 0)
    val v2 = Vector3D(3, 4, 0)
    val dist = v1.distanceTo(v2)()
    assert(math.abs(dist.value - 5.0) < 1e-10)
    assert(dist.isInstanceOf[DistanceUnit.Meter])
  }

  test("Vector3D normalize") {
    val v = Vector3D(0, 3, 4)
    val n = v.normalize
    assert(n.magnitude === 1)
  }

  test("Vector3D angleTo") {
    val v1 = Vector3D(1, 0, 0)
    val v2 = Vector3D(0, 1, 0)
    val angle = v1.angleTo(v2)()
    assert(math.abs(angle.value - (Math.PI / 2)) < 1e-10)
    assert(angle.isInstanceOf[AngleUnit.Radian])
  }

  test("Vector3D toString") {
    val v = Vector3D(1, 2, 3)
    assert(v.toString === "Vector3D(1.0, 2.0, 3.0) [m]")
  }

  test("Vector3D Zero") {
    assert(Vector3D.Zero === Vector3D(0, 0, 0))
  }

  test("Vector3D fromArray and apply with DistanceUnit") {
    val arr = Array(DistanceUnit.Meter(1), DistanceUnit.Meter(2), DistanceUnit.Meter(3))
    val v = Vector3D.fromArray(arr)
    assert(v === Vector3D(1, 2, 3))
    val v2 = Vector3D(DistanceUnit.Kilometer(1), DistanceUnit.Meter(500), DistanceUnit.Meter(0))
    assert(math.abs(v2.x - 1000.0) < 1e-10)
    assert(math.abs(v2.y - 500.0) < 1e-10)
    assert(math.abs(v2.z - 0.0) < 1e-10)
  }

  test("Vector3D BinOp +, -, *, /, ~, dot, X") {
    val v1 = Vector3D(1, 2, 3)
    val v2 = Vector3D(4, 5, 6)
    assert((v1 + v2) === Vector3D(5, 7, 9))
    assert((v2 - v1) === Vector3D(3, 3, 3))
    assert((v1 * 2) === Vector3D(2, 4, 6))
    assert((v2 / 2) === Vector3D(2, 2.5, 3))
    assert((v1 ~ Vector3D(1, 2, 3)))
    assert(!(v1 ~ Vector3D(1, 2, 3.1)))
    assert(v1.dot(v2) === 32.0)
    assert((v1 X v2) === Vector3D(-3, 6, -3))
  }
  
}

