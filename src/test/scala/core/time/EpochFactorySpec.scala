package org.abh80.nf
package core.time

import org.scalatest.funsuite.AnyFunSuite

class EpochFactorySpec extends AnyFunSuite {
  private val tt = TimeScaleFactory.getTT
  private val utc = TimeScaleFactory.getUTC

  test("standard epochs") {
    assertResult("-4712-01-01T12:00:00.000")(EpochFactory.JULIAN.toString(tt))
    assertResult("1970-01-01T00:00:00.000")(EpochFactory.UNIX.toString(utc))
    assertResult("2000-01-01T12:00:00.000")(EpochFactory.J2000_0.toString(tt))
    assertResult("1980-01-06T00:00:00.000")(EpochFactory.GPS.toString(utc))
    assertResult("1998-01-01T00:00:00.000")(EpochFactory.CXCSEC.toString(tt))
    assertResult("1900-01-01T12:00:00.000")(EpochFactory.J1900.toString(tt))
  }
}
