package org.abh80.nf
package core.time

/**
 * A time scale that applies a fixed offset to TAI.
 *
 * @param name   The name of this time scale.
 * @param offset The fixed offset to apply to TAI. This offset represents the difference between the target time scale and TAI.
 *               For example, if the target time scale is always 10 seconds ahead of TAI, then the offset should be +10 seconds.
 */
class FixedTimeOffsetScale(override protected val name: String, val offset: TimeFormat) extends TimeScale {
  /**
   * Gets the time past TAI for this time scale.
   *
   * @return The fixed offset, representing the time past TAI.
   */
  override def timePastTAI(date: Date): TimeFormat = offset

  /**
   * Gets the time to TAI from this time scale.
   *
   * @return The negation of the fixed offset, representing the time to TAI.
   */
  override def timeToTAI(date: Date): TimeFormat = offset.negate()
}