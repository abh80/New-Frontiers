package org.abh80.nf
package core.time

import core.Constants

import scala.math.floor

/**
 * GLONASS scale implementation
 * GLONASS is a fixed offset from UTC + 3 hours
 *
 * @param utc UTC scale
 */
class GLONASSScale(utc: TimeScale) extends TimeScale {

  // The 3 hours offset
  private val offset = TimeFormat.fromTimeUnit(3L, TimeUnit.HOURS)

  /** The name identifier of this time scale */
  override protected val name: String = "GLONASS"

  /**
   * Calculates the difference between this time scale and TAI at the given time.
   *
   * @param time The absolute time to calculate the offset for
   * @return A TimeFormat representing the offset from TAI
   */
  override def timePastTAI(time: AbsoluteTime): TimeFormat = offset + utc.timePastTAI(time)

  override def timeToTAI(date: Date, time: Time): TimeFormat = {
    // 1. Calculate the total seconds in the day, adjusted ONCE to get to UTC.
    var utcSecondsInDay = time.getSecondsInDayObject() - offset

    // 2. Determine the day rollover based on the adjusted seconds.
    // Using SECONDS_IN_A_JULIAN_DAY is standard for this.
    val dayOffset = floor(utcSecondsInDay.toDouble / Constants.SECONDS_IN_A_JULIAN_DAY).toInt

    // 3. Create the final UTC date and time.
    val utcDate = Date(date.getJ2000Day + dayOffset)

    // Normalize seconds to be within a single day for the Time object.
    if (dayOffset != 0) {
      utcSecondsInDay = utcSecondsInDay - TimeFormat(dayOffset * TimeFormat.DAY.getSeconds, 0L)
    }
    val utcTime = Time(utcSecondsInDay)

    utc.timeToTAI(utcDate, utcTime) - offset
  }

  override def isInsideLeapSecond(time: AbsoluteTime): Boolean = utc.isInsideLeapSecond(time)

  override def minuteDuration(time: AbsoluteTime): Int = utc.minuteDuration(time)

  override def getLeap(time: AbsoluteTime): TimeFormat = utc.getLeap(time)
}
