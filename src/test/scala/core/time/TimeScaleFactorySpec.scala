package org.abh80.nf
package core.time

import org.scalatest.funsuite.AnyFunSuite

class TimeScaleFactorySpec extends AnyFunSuite {
  test("factory accessors return singleton instances") {
    assert(TimeScaleFactory.getTAI eq TimeScaleFactory.getTAI)
    assert(TimeScaleFactory.getUTC eq TimeScaleFactory.getUTC)
    assert(TimeScaleFactory.getTT eq TimeScaleFactory.getTT)
    assert(TimeScaleFactory.getTDB eq TimeScaleFactory.getTDB)
    assert(TimeScaleFactory.getTDT eq TimeScaleFactory.getTDT)
    assert(TimeScaleFactory.getGLONASS eq TimeScaleFactory.getGLONASS)
    assert(TimeScaleFactory.getIRNSS eq TimeScaleFactory.getIRNSS)
    assert(TimeScaleFactory.getGPS eq TimeScaleFactory.getGPS)
  }
}
