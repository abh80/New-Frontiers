package org.abh80.nf
package core.time

import core.time.EpochFactory.J2000_0
import util.DateUtil.Month
import java.time.Instant

/**
 * Represents an absolute point in time based on a given time format and timescale.
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
   * Creates an `AbsoluteTime` instance using the J2000 epoch (January 1, 2000, 12:00 TT).
   * The J2000 epoch is represented in seconds and attoseconds.
   */
  def this() = {
    this(TimeFormat(J2000_0.getSeconds, J2000_0.getAttoSeconds))
  }

  /**
   * Constructor to create an `AbsoluteTime` instance using a `Date`, `Time`, and `TimeScale`.
   *
   * @param date  the date component
   * @param time  the time component
   * @param scale the time scale used to convert the time to TAI (International Atomic Time)
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
   * @throws IllegalArgumentException if the input values are invalid
   */
  @throws[IllegalArgumentException]
  def this(year: Int, month: Int, day: Int, hour: Int, minute: Int, seconds: TimeFormat, scale: TimeScale) =
    this(Date(year, month, day), Time(hour, minute, seconds), scale)

  /**
   * Constructor to create an `AbsoluteTime` instance using year, month, day, hour, minute,
   * seconds (as `Double`), and a `TimeScale`.
   *
   * @throws IllegalArgumentException if the input values are invalid
   */
  @throws[IllegalArgumentException]
  def this(year: Int, month: Int, day: Int, hour: Int, minute: Int, seconds: Double, scale: TimeScale) =
    this(year, month, day, hour, minute, TimeFormat.fromDouble(seconds), scale)

  /**
   * Constructor to create an `AbsoluteTime` instance using year, month (as `Month` enum), day,
   * hour, minute, seconds (as `TimeFormat`), and a `TimeScale`.
   *
   * @throws IllegalArgumentException if the input values are invalid
   */
  @throws[IllegalArgumentException]
  def this(year: Int, month: Month, day: Int, hour: Int, minute: Int, seconds: TimeFormat, scale: TimeScale) =
    this(year, month.getIntegerValue, day, hour, minute, seconds, scale)

  /**
   * Constructor to create an `AbsoluteTime` instance using year, month (as `Month` enum), day,
   * hour, minute, seconds (as `Double`), and a `TimeScale`.
   *
   * @throws IllegalArgumentException if the input values are invalid
   */
  @throws[IllegalArgumentException]
  def this(year: Int, month: Month, day: Int, hour: Int, minute: Int, seconds: Double, scale: TimeScale) =
    this(year, month, day, hour, minute, TimeFormat.fromDouble(seconds), scale)

  /**
   * Constructor to create an `AbsoluteTime` instance using a `Date` and a `TimeScale`.
   * The time component is set to 00:00:00 by default.
   *
   * @param date  the date component
   * @param scale the time scale used to convert the time to TAI
   */
  def this(date: Date, scale: TimeScale) =
    this(date, Time(0, 0, TimeFormat.Zero), scale)

  /**
   * Constructor to create an `AbsoluteTime` instance using year, month, day, and a `TimeScale`.
   * The time component is set to 00:00:00 by default.
   *
   * @throws IllegalArgumentException if the input values are invalid
   */
  @throws[IllegalArgumentException]
  def this(year: Int, month: Int, day: Int, scale: TimeScale) =
    this(Date(year, month, day), scale)

  // TODO: add instant constructor after UTC

  def durationFrom(time: AbsoluteTime): TimeFormat = this - time

  def getTimeObject(scale: TimeScale): Time =
    val timeOffset = this + scale.timePastTAI(this)
    val j2000_shifted = timeOffset.getSeconds + 42000L

    var sod = j2000_shifted % 86400L
    if sod < 0L then sod += 86400L

    Time(sod)

  def getDateObject(scale: TimeScale): Date =
    val timeOffset = this + scale.timePastTAI(this)
    val j2000_shifted = timeOffset.getSeconds + 42000L

    val date = (j2000_shifted - getTimeObject(scale).getSecondsInDay / 86400L).toInt

    Date(Date.J2000_0, date)

  def getTimeObject(utcOffset: Int): Time =
    ???
}

object AbsoluteTime {
  implicit class BinOp(self: AbsoluteTime) {
    def ++(other: TimeFormat): AbsoluteTime =
      AbsoluteTime(self + other)

    def ++(shiftBy: Long) : AbsoluteTime =
      AbsoluteTime(self + TimeFormat.fromDouble(shiftBy.toDouble))
  }

  def fromMJDDate(mjd: Int, timeScale: TimeScale, seconds: TimeFormat = TimeFormat.Zero): AbsoluteTime =
    new AbsoluteTime(Date(Date.MJD, mjd), Time(seconds), timeScale)
}
