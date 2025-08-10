package org.abh80.nf
package core.time

import breeze.numerics.*
import org.abh80.nf.core.metrics.AngleUnit

private val G0 = AngleUnit.Degree(357.53).toRadians
private val G1 = AngleUnit.Degree(0.9856003).toRadians

private val F1 = 0.001658
private val F2 = 0.000028


// credits: https://gssc.esa.int/navipedia/index.php/Transformations_between_Time_Systems#TAI_-_TDT,_TCG,_TT
class TDBScale(val tt: TimeScale, j2000Epoch: AbsoluteTime = EpochFactory.J2000_0) extends TimeScale {
  override protected val name: String = "TDB"

  override def timePastTAI(date: AbsoluteTime): TimeFormat =
    val jdtt = date.durationFrom(j2000Epoch).toDouble / 86400.0

    val g = G0 + G1 * jdtt

    tt.timePastTAI(date) + TimeFormat.fromDouble(sin(g) * (F1 + F2 * cos(g)))
}
