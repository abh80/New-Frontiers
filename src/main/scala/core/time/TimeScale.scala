package org.abh80.nf
package core.time

trait TimeScale {
  protected val name: String

  val minuteDuration: Int = 60

  def timePastTAI(time: AbsoluteTime): TimeFormat

  def timeToTAI(date: Date, time: Time): TimeFormat =
    val ref = new AbsoluteTime(date, time, TAIScale())
    var offset = TimeFormat.Zero

    for (i <- 1 until 5) {
      offset = timePastTAI(ref ++ offset).negate()
    }
    
    offset

  def getLastLeapSecond: TimeFormat = TimeFormat.Zero

  def getName: String = name
}
