package org.abh80.nf
package core.time

import breeze.linalg.*
import breeze.numerics.*

private val G0 = 6.24004077 // 357.528 degrees in radians
private val G1 = 0.01720197 // 0.9856003

private val F1 = 0.001658
private val F2 = 0.000028

class TDBScale(val tt: TimeScale, j2000Epoch: Date = EpochStoreFactory.J2000_0) extends TimeScale {
  override protected val name: String = "TDB"

  override def timePastTAI(date: Date): TimeFormat =
    ???

  override def timeToTAI(date: Date): TimeFormat = ???
}
