package org.abh80.nf
package core.time

import core.time.EpochFactory.J2000_0
import util.DateUtil.Month

import org.abh80.nf.core.Constants

import java.time.Instant

/**
 * Represents an absolute point in time based on a given time format and timescale.
 *
 * Internally, time is represented as a [[TimeFormat]] (seconds + attoseconds)
 * relative to the J2000 epoch (January 1, 2000, 12:00 TT).
 *
 * ==Mathematical Background==
 * - The J2000 epoch is defined as **JD 2451545.0 TT** (Julian Date).
 * - Conversion between calendar date and J2000 offset is done by:
 *   {{{
 *   seconds = (days_since_J2000 * 86400) + time_of_day_seconds
 *   }}}
 * - Leap seconds and timescale offsets (e.g., UTC → TAI → TT) are applied
 *   using [[TimeScale]] transformations.
 *
 * @param tf the underlying time format instance used to represent the absolute time
 * @see TimeFormat
 * @see TimeScale
 */
class AbsoluteTime(tf: TimeFormat)
  extends TimeFormat(tf)
    with Comparable[TimeFormat]
    with Serializable {

  /**
   * Default constructor.
   * Creates an `AbsoluteTime` instance using the J2000 epoch
   * (January 1, 2000, 12:00 TT).
   *
   * ==Math Note==
   * - J2000 epoch corresponds to **JD 2451545.0 TT**.
   * - This is exactly 12:00 Terrestrial Time on Jan 1, 2000.
   */
  def this() = {
    this(TimeFormat(J2000_0.getSeconds, J2000_0.getAttoSeconds))
  }

  /**
   * Constructor to create an `AbsoluteTime` instance using a `Date`, `Time`, and `TimeScale`.
   *
   * ==Math Note==
   * - Days since J2000 are converted to seconds:
   *   {{{
   *   total_seconds = (days_since_J2000 * 86400)
   *                 + (hours * 3600)
   *                 + (minutes * 60)
   *                 + seconds
   *   }}}
   * - UTC offset and leap seconds are applied via [[TimeScale.timeToTAI]].
   *
   * @param date  the date component
   * @param time  the time component
   * @param scale the time scale used to convert the time to TAI
   */
  def this(date: Date, time: Time, scale: TimeScale) =
    this(
      TimeFormat(
        (date.getJ2000Day * 24L + time.getHour) * 3600L +
          (time.getMinute - time.getUtcOffset - 720) * 60,
        0L
      ) + time.getSecondsAsTimeFormat + scale.timeToTAI(date, time)
    )

  /**
   * Constructor to create an `AbsoluteTime` instance using year, month, day, hour, minute,
   * seconds (as `TimeFormat`), and a `TimeScale`.
   *
   * ==Math Note==
   * - Calendar date → Julian Day Number (JDN) → J2000 offset.
   * - J2000 offset = (JDN - 2451545.0) * 86400.
   *
   * @throws IllegalArgumentException if the input values are invalid
   */
  @throws[IllegalArgumentException]
  def this(
            year: Int,
            month: Int,
            day: Int,
            hour: Int,
            minute: Int,
            seconds: TimeFormat,
            scale: TimeScale
          ) =
    this(Date(year, month, day), Time(hour, minute, seconds), scale)

  /**
   * Constructor to create an `AbsoluteTime` instance using year, month, day, hour, minute,
   * seconds (as `Double`), and a `TimeScale`.
   *
   * ==Math Note==
   * - Seconds are converted to [[TimeFormat]] with attosecond precision.
   *
   * @throws IllegalArgumentException if the input values are invalid
   */
  @throws[IllegalArgumentException]
  def this(
            year: Int,
            month: Int,
            day: Int,
            hour: Int,
            minute: Int,
            seconds: Double,
            scale: TimeScale
          ) =
    this(year, month, day, hour, minute, TimeFormat.fromDouble(seconds), scale)

  /**
   * Constructor to create an `AbsoluteTime` instance using year, month (as `Month` enum), day,
   * hour, minute, seconds (as `TimeFormat`), and a `TimeScale`.
   *
   * ==Math Note==
   * - Uses [[Month.getIntegerValue]] to map enum → integer month.
   */
  @throws[IllegalArgumentException]
  def this(
            year: Int,
            month: Month,
            day: Int,
            hour: Int,
            minute: Int,
            seconds: TimeFormat,
            scale: TimeScale
          ) =
    this(year, month.getIntegerValue, day, hour, minute, seconds, scale)

  /**
   * Constructor to create an `AbsoluteTime` instance using year, month (as `Month` enum), day,
   * hour, minute, seconds (as `Double`), and a `TimeScale`.
   */
  @throws[IllegalArgumentException]
  def this(
            year: Int,
            month: Month,
            day: Int,
            hour: Int,
            minute: Int,
            seconds: Double,
            scale: TimeScale
          ) =
    this(year, month, day, hour, minute, TimeFormat.fromDouble(seconds), scale)

  /**
   * Constructor to create an `AbsoluteTime` instance using a `Date` and a `TimeScale`.
   * The time component is set to 00:00:00 by default.
   */
  def this(date: Date, scale: TimeScale) =
    this(date, Time(0, 0, TimeFormat.Zero), scale)

  /**
   * Constructor to create an `AbsoluteTime` instance using year, month, day, and a `TimeScale`.
   * The time component is set to 00:00:00 by default.
   */
  @throws[IllegalArgumentException]
  def this(year: Int, month: Int, day: Int, scale: TimeScale) =
    this(Date(year, month, day), scale)

  /**
   * Constructor to create an `AbsoluteTime` instance from a Java [[Instant]] and a given timescale.
   *
   * ==Math Note==
   * - Java Instant is always UTC-based.
   * - Conversion: `epoch_seconds + nanos → TimeFormat`.
   */
  def this(instant: Instant, scale: TimeScale) =
    this(new Date(instant), Time(instant), scale)

  /**
   * Constructor to create an `AbsoluteTime` instance from a Java [[Instant]].
   * Uses UTC as the default timescale.
   */
  def this(instant: Instant) =
    this(instant, TimeScaleFactory.getUTC)

  /**
   * Computes the duration between this time and another `AbsoluteTime`.
   *
   * ==Math Note==
   * - Duration = Δt = (this.seconds - other.seconds).
   */
  def durationFrom(time: AbsoluteTime): TimeFormat = this - time

  /**
   * Returns the ISO-8601 string representation of this time in UTC.
   * Appends "Z" to indicate UTC.
   */
  override def toString: String =
    toString(TimeScaleFactory.getUTC) ++ "Z"

  /**
   * Returns the ISO-8601 string representation of this time in the given timescale.
   *
   * ==Math Note==
   * - Format: `YYYY-MM-DDTHH:MM:SS[.fraction]`
   */
  def toString(scale: TimeScale): String =
    val (d, t) = getDateTime(scale)
    d.toString ++ "T" ++ t.toISO8601StringTrimmed()

  /**
   * Converts this absolute time into a `(Date, Time)` pair in the given timescale.
   *
   * ==Math Note==
   * - Shift by 43200 seconds (12h) because J2000 epoch is defined at **12:00 TT**,
   *   not midnight.
   * - Day calculation:
   *   {{{
   *   j2000_shifted = seconds_since_J2000 + 43200
   *   day_index     = floor(j2000_shifted / 86400)
   *   time_of_day   = j2000_shifted mod 86400
   *   }}}
   * - Leap seconds are applied if the instant falls inside a leap second.
   *
   * @param scale the timescale to use
   * @return a tuple of (Date, Time)
   */
  def getDateTime(scale: TimeScale): (Date, Time) =
    val timeOffset = this + scale.timePastTAI(this)
    val j2000_shifted = timeOffset.getSeconds + 43200L

    var time = j2000_shifted % Constants.SECONDS_IN_A_JULIAN_DAY
    if time < 0L then time += Constants.SECONDS_IN_A_JULIAN_DAY

    val date = ((j2000_shifted - time) / Constants.SECONDS_IN_A_JULIAN_DAY).toInt

    val leap =
      if scale.isInsideLeapSecond(this) then scale.getLeap(this)
      else TimeFormat.Zero

    (
      Date(Date.J2000_0, date),
      Time(
        TimeFormat(time, timeOffset.getAttoSeconds),
        leap,
        scale.minuteDuration(this)
      )
    )

  /**
   * Computes the offset between two time scales at this absolute time.
   *
   * ==Math Note==
   * - Offset = Δ = (scale1 - TAI) - (scale2 - TAI).
   * - Example: UTC vs TAI → difference = leap seconds.
   *
   * @param scale1 the first timescale
   * @param scale2 the second timescale
   * @return the offset between the two timescales as a [[TimeFormat]]
   */
  def offsetBetween(scale1: TimeScale, scale2: TimeScale): TimeFormat =
    scale1.timePastTAI(this) - scale2.timePastTAI(this)

}

object AbsoluteTime {

  /**
   * Implicit class providing convenient operators for shifting an `AbsoluteTime`
   * by a given duration.
   *
   * ==Math Note==
   * - Shifting is equivalent to:
   *   {{{
   *   new_time = old_time + Δt
   *   }}}
   */
  implicit class BinOp(self: AbsoluteTime) {

    /** Shift by a [[TimeFormat]] duration. */
    def ++(other: TimeFormat): AbsoluteTime =
      AbsoluteTime(self + other)

    /** Shift by a number of seconds (Long). */
    def ++(shiftBy: Long): AbsoluteTime =
      ++(shiftBy.toDouble)

    /** Shift by a number of seconds (Double). */
    def ++(shiftBy: Double): AbsoluteTime =
      AbsoluteTime(self + TimeFormat.fromDouble(shiftBy))
  }

  /**
   * Creates an `AbsoluteTime` from a Modified Julian Date (MJD).
   *
   * ==Math Note==
   * - MJD = JD - 2400000.5
   * - Conversion:
   *   {{{
   *   JD = MJD + 2400000.5
   *   seconds_since_J2000 = (JD - 2451545.0) * 86400
   *   }}}
   *
   * @param mjd      the Modified Julian Date
   * @param timeScale the timescale to use
   * @param seconds   optional seconds offset (default = 0)
   * @return an `AbsoluteTime` corresponding to the given MJD
   */
  def fromMJDDate(
                   mjd: Int,
                   timeScale: TimeScale,
                   seconds: TimeFormat = TimeFormat.Zero
                 ): AbsoluteTime =
    new AbsoluteTime(Date(Date.MJD, mjd), Time(seconds), timeScale)

  /**
   * Returns the current system time as an `AbsoluteTime` in UTC.
   *
   * ==Math Note==
   * - Uses [[Instant.now]] (system clock, UTC).
   * - Conversion: epoch_seconds → J2000 offset.
   */
  def now(): AbsoluteTime = new AbsoluteTime(Instant.now())
}
