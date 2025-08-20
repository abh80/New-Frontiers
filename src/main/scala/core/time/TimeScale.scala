package org.abh80.nf
package core.time

/**
 * A trait representing a time scale system for astronomical time calculations.
 *
 * TimeScale provides mechanisms to convert between different time standards,
 * with TAI (International Atomic Time) serving as a reference scale.
 * Time scales may handle leap seconds and other time adjustments differently.
 *
 * Common implementations include UTC, TAI, TT, etc.
 */
trait TimeScale {
  /** The name identifier of this time scale */
  protected val name: String

  /**
   * Duration of a minute in seconds.
   * Default is 60 seconds for most time scales.
   * May differ for specialized astronomical time scales.
   */
  def minuteDuration(time: AbsoluteTime): Int = 60

  /**
   * Calculates the difference between this time scale and TAI at the given time.
   *
   * @param time The absolute time to calculate the offset for
   * @return A TimeFormat representing the offset from TAI
   */
  def timePastTAI(time: AbsoluteTime): TimeFormat

  /**
   * Converts a date and time in this scale to the equivalent TAI time.
   *
   * This method iteratively converges on the correct offset using the timePastTAI function.
   * It typically requires 3-4 iterations to converge with sufficient accuracy.
   *
   * @param date The date component in this time scale
   * @param time The time component in this time scale
   * @return The offset to add to the input time to get TAI
   */
  def timeToTAI(date: Date, time: Time): TimeFormat =
    val ref = new AbsoluteTime(date, time, TAIScale())
    var offset = TimeFormat.Zero

    // Iteratively refine the conversion
    for (i <- 1 until 8) {
      offset = timePastTAI(ref ++ offset).negate()

    }
    
    offset

  /**
   * Returns the most recent leap second adjustment applied to this time scale.
   *
   * For time scales without leap seconds, this returns zero.
   *
   * @return The most recent leap second adjustment as a TimeFormat
   */
  def getLastLeapSecond: TimeFormat = TimeFormat.Zero

  /**
   * Gets the identifying name of this time scale.
   *
   * @return The name of this time scale (e.g., "UTC", "TAI", "TT")
   */
  def getName: String = name

  /**
   * Determines if a given absolute time falls within a leap second.
   *
   * For time scales that don't use leap seconds (like TAI), this always returns false.
   * For UTC, this would return true during leap second events.
   *
   * @param time The absolute time to check
   * @return true if the time is inside a leap second, false otherwise
   */
  def isInsideLeapSecond(time: AbsoluteTime): Boolean = false

  /**
   * Retrieves the leap second adjustment for the given absolute time.
   *
   * @param time The absolute time for which to determine the leap second adjustment.
   * @return The leap second adjustment as a TimeFormat instance.
   */
  def getLeap(time: AbsoluteTime): TimeFormat = TimeFormat.Zero

  override def toString: String = name
}
