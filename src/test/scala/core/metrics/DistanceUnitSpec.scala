package org.abh80.nf.core.metrics

import org.scalatest.funsuite.AnyFunSuite

class DistanceUnitSpec extends AnyFunSuite {
  import DistanceUnit._

  test("Meter toMeter and fromMeter") {
    val m = Meter(42.0)
    assert(m.toMeter === 42.0)
    assert(m.fromMeter(100.0) === Meter(100.0))
    assert(m.symbol === "m")
    assert(m.toString === "42.0 m")
  }

  test("Kilometer toMeter and fromMeter") {
    val km = Kilometer(1.5)
    assert(km.toMeter === 1500.0)
    assert(km.fromMeter(2000.0) === Kilometer(2.0))
    assert(km.symbol === "km")
    assert(km.toString === "1.5 km")
  }

  test("AstronomicalUnit toMeter and fromMeter") {
    val au = AstronomicalUnit(2.0)
    val AU_TO_METERS = 1.495978707e11
    assert(au.toMeter === 2.0 * AU_TO_METERS)
    assert(au.fromMeter(AU_TO_METERS) === AstronomicalUnit(1.0))
    assert(au.symbol === "AU")
    assert(au.toString === "2.0 AU")
  }

  test("DistanceUnit BinOp +, -, ~, *, /") {
    val m1 = Meter(100.0)
    val m2 = Meter(50.0)
    assert((m1 + m2) === Meter(150.0))
    assert((m1 - m2) === Meter(50.0))
    assert((m1 ~ Meter(100.0)))
    assert(!(m1 ~ Meter(101.0)))
    assert((m1 * Meter(2.0)) === Meter(200.0))
    assert((m1 / Meter(2.0)) === Meter(50.0))
  }

  test("fromMeters utility") {
    val km = DistanceUnit.fromMeters(3000.0, Kilometer.fromMeter)
    assert(km === Kilometer(3.0))
  }
}

