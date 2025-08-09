package org.abh80.nf
package core.time

import breeze.numerics.*

private val G0 = 6.24004077 // 357.528 degrees in radians
private val G1 = 0.01720197 // 0.9856003

private val F1 = 0.001658
private val F2 = 0.000028


// credits: https://gssc.esa.int/navipedia/index.php/Transformations_between_Time_Systems#TAI_-_TDT,_TCG,_TT
class TDBScale(val tt: TimeScale, j2000Epoch: AbsoluteTime = EpochFactory.J2000_0) extends TimeScale {
  override protected val name: String = "TDB"

  override def timePastTAI(date: AbsoluteTime): TimeFormat =
    val jdtt = date.durationFrom(j2000Epoch).toDouble / 86400.0

    val g = G0 + G1 * (jdtt - 2451545.0)

    tt.timePastTAI(date) + TimeFormat.fromDouble(F1 * sin(g) + F2 * sin(2 * g))
}
