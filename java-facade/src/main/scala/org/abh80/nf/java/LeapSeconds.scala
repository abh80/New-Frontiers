package org.abh80.nf.java

import org.abh80.nf.core.time.{Date, TimeScale, UTCScale}

/**
 * Java-idiomatic facade for constructing [[org.abh80.nf.core.time.UTCScale.LeapSecondOffset]].
 *
 * The Scala constructor takes an implicit `TimeScale` parameter, which Java cannot supply
 * positionally. This helper accepts the TAI scale as an explicit argument.
 */
object LeapSeconds {

  /**
   * Build a leap second offset record.
   *
   * @param tai         the TAI time scale (typically `TimeScaleFactory.getTAI()`)
   * @param startDate   the start date of the offset's validity
   * @param mjdBase     the MJD base for the linear-drift computation
   * @param fixedOffset the fixed offset in seconds
   * @param rate        the per-day rate of additional drift (s/day)
   */
  def create(
    tai: TimeScale,
    startDate: Date,
    mjdBase: Int,
    fixedOffset: Double,
    rate: Double
  ): UTCScale.LeapSecondOffset = {
    implicit val tScale: TimeScale = tai
    UTCScale.LeapSecondOffset(startDate, mjdBase, fixedOffset, rate)
  }
}
