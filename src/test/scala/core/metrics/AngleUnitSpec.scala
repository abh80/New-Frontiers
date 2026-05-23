package org.abh80.nf.core.metrics

import org.scalatest.funsuite.AnyFunSuite

class AngleUnitSpec extends AnyFunSuite {
  import AngleUnit._

  test("Radian toRadian and fromRadians") {
    val rad = Radian(2.0)
    assert(rad.toRadians === 2.0)
    assert(rad.fromRadians(1.0) === Radian(1.0))
    assert(rad.symbol === "rad")
    assert(rad.toString === "2.0 rad")
  }

  test("Degree toRadian and fromRadians") {
    val deg = Degree(180.0)
    assert(deg.toRadians === Math.PI)
    assert(deg.fromRadians(Math.PI) === Degree(180.0))
    assert(deg.symbol === "°")
    assert(deg.toString === "180.0 °")
  }

  test("ArcMinute toRadian and fromRadians") {
    val amin = ArcMinute(60.0)
    assert(math.abs(amin.toRadians - (Math.PI / 180.0)) < 1e-10)
    assert(math.abs(amin.fromRadians(Math.PI / 180.0).value - 60.0) < 1e-10)
    assert(amin.symbol === "′")
    assert(amin.toString === "60.0 ′")
  }

  test("ArcSecond toRadian and fromRadians") {
    val asec = ArcSecond(3600.0)
    assert(math.abs(asec.toRadians - (Math.PI / 180.0)) < 1e-10)
    assert(math.abs(asec.fromRadians(Math.PI / 180.0).value - 3600.0) < 1e-10)
    assert(asec.symbol === "″")
    assert(asec.toString === "3600.0 ″")
  }

  test("HourAngle toRadian and fromRadians") {
    val ha = HourAngle(12.0)
    assert(ha.toRadians === Math.PI)
    assert(ha.fromRadians(Math.PI) === HourAngle(12.0))
    assert(ha.symbol === "h")
    assert(ha.toString === "12.0 h")
  }

  test("fromRadians utility") {
    val deg = AngleUnit.fromRadians(Math.PI, Degree.fromRadians)
    assert(math.abs(deg.value - 180.0) < 1e-10)
  }

  test("BinOp * scales by scalar; ratio gives dimensionless result") {
    val rad90 = Radian(math.Pi / 2)
    assert(math.abs((rad90 * 2.0).toRadians - math.Pi) < 1e-10)
    assert(math.abs((rad90 * 0.5).toRadians - math.Pi / 4) < 1e-10)

    val deg180 = Degree(180.0)
    val deg90  = Degree(90.0)
    assert(math.abs(deg180.ratio(deg90) - 2.0) < 1e-10)
    assert(math.abs(deg90.ratio(deg180) - 0.5) < 1e-10)

    // ratio is dimensionless — result is a plain Double, not an angle
    val r: Double = deg180.ratio(deg90)
    assert(r === 2.0)
  }
}

