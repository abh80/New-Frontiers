package org.abh80.nf
package core.time

trait TimeScale {
  protected val name: String

  val minuteDuration: Int = 60

  def timePastTAI(date: Date): TimeFormat

  def timeToTAI(date: Date): TimeFormat

  def getLastLeapSecond: TimeFormat = TimeFormat.Zero

  def getName: String = name
}
