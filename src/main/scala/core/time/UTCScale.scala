package org.abh80.nf
package core.time

import org.abh80.nf.core.Constants

import scala.annotation.switch

private val NANOS_IN_SECONDS = 1_000_000_000
private val SECONDS_IN_DAY = Constants.SECONDS_IN_A_JULIAN_DAY

// credits: https://hpiers.obspm.fr/eoppc/bul/bulc/UTC-TAI.history

/**
 * Implements the UTC (Coordinated Universal Time) time scale, including leap second handling.
 *
 * This class provides conversion between TAI (International Atomic Time) and UTC,
 * accounting for historical and current leap second definitions. It maintains a list
 * of leap second offsets and computes the appropriate offset for any given time.
 *
 * @param tai         The TAI time scale instance used as a reference for conversions.
 * @param leapOffsets The leap offsets after Jan 1. 2017. Likely to be deprecated after 2035, since the leap seconds will be dropped by then.
 * @example
 * {{{
 *                    val leapOffsets = Array(
 *                      LeapSecondOffset(Date(2024, 1, 1), 0, 38, 0),
 *                      LeapSecondOffset(Date(2025, 7, 1), 0, 39, 0)
 *                    )
 *
 *                    val scale = UTCScale(TimeScaleFactory.getTAI, leapOffsets)
 * }}}
 * @see [[TimeScale]]
 */
class UTCScale(tai: TimeScale, leapOffsets: Array[UTCScale.LeapSecondOffset] = Array()) extends TimeScale {

  import UTCScale.{ComputedLeapSecondOffset, LeapSecondOffset}

  private val fixedLeapOffsets = {
    implicit val t: TimeScale = tai

    Array(
      LeapSecondOffset(Date(1961, 1, 1), 37300, 1.4228180, 0.001_296),
      LeapSecondOffset(Date(1961, 8, 1), 37300, 1.3728180, 0.001_296),
      LeapSecondOffset(Date(1962, 1, 1), 37665, 1.8458580, 0.001_123_2),
      LeapSecondOffset(Date(1963, 11, 1), 37665, 1.9458580, 0.001_123_2),
      LeapSecondOffset(Date(1964, 1, 1), 38761, 3.2401300, 0.001_296),
      LeapSecondOffset(Date(1964, 4, 1), 38761, 3.3401300, 0.001_296),
      LeapSecondOffset(Date(1964, 9, 1), 38761, 3.4401300, 0.001_296),
      LeapSecondOffset(Date(1965, 1, 1), 38761, 3.5401300, 0.001_296),
      LeapSecondOffset(Date(1965, 3, 1), 38761, 3.6401300, 0.001_296),
      LeapSecondOffset(Date(1965, 7, 1), 38761, 3.7401300, 0.001_296),
      LeapSecondOffset(Date(1965, 9, 1), 38761, 3.8401300, 0.001_296),
      LeapSecondOffset(Date(1966, 1, 1), 39126, 4.3131700, 0.002_592),
      LeapSecondOffset(Date(1968, 2, 1), 39126, 4.2131700, 0.002_592),
      LeapSecondOffset(Date(1972, 1, 1), 41317, 10, 0),
      LeapSecondOffset(Date(1972, 7, 1), 41317, 11, 0),
      LeapSecondOffset(Date(1973, 1, 1), 41317, 12, 0),
      LeapSecondOffset(Date(1974, 1, 1), 41317, 13, 0),
      LeapSecondOffset(Date(1975, 1, 1), 41317, 14, 0),
      LeapSecondOffset(Date(1976, 1, 1), 41317, 15, 0),
      LeapSecondOffset(Date(1977, 1, 1), 41317, 16, 0),
      LeapSecondOffset(Date(1978, 1, 1), 41317, 17, 0),
      LeapSecondOffset(Date(1979, 1, 1), 41317, 18, 0),
      LeapSecondOffset(Date(1980, 1, 1), 41317, 19, 0),
      LeapSecondOffset(Date(1981, 7, 1), 41317, 20, 0),
      LeapSecondOffset(Date(1982, 7, 1), 41317, 21, 0),
      LeapSecondOffset(Date(1983, 7, 1), 41317, 22, 0),
      LeapSecondOffset(Date(1985, 7, 1), 41317, 23, 0),
      LeapSecondOffset(Date(1988, 1, 1), 41317, 24, 0),
      LeapSecondOffset(Date(1990, 1, 1), 41317, 25, 0),
      LeapSecondOffset(Date(1991, 1, 1), 41317, 26, 0),
      LeapSecondOffset(Date(1992, 7, 1), 41317, 27, 0),
      LeapSecondOffset(Date(1993, 7, 1), 41317, 28, 0),
      LeapSecondOffset(Date(1994, 7, 1), 41317, 29, 0),
      LeapSecondOffset(Date(1996, 1, 1), 41317, 30, 0),
      LeapSecondOffset(Date(1997, 7, 1), 41317, 31, 0),
      LeapSecondOffset(Date(1999, 1, 1), 41317, 32, 0),
      LeapSecondOffset(Date(2006, 1, 1), 41317, 33, 0),
      LeapSecondOffset(Date(2009, 1, 1), 41317, 34, 0),
      LeapSecondOffset(Date(2012, 7, 1), 41317, 35, 0),
      LeapSecondOffset(Date(2015, 7, 1), 41317, 36, 0),
      LeapSecondOffset(Date(2017, 1, 1), 41317, 37, 0)
    ) ++ leapOffsets
  }

  /** Adjusted to previous offsets with respect to TAI */
  private val computedLeapOffsets: Array[ComputedLeapSecondOffset] = {
    val (built, _) = fixedLeapOffsets.foldLeft[(List[ComputedLeapSecondOffset], ComputedLeapSecondOffset)](
      (List.empty[ComputedLeapSecondOffset], null)
    ) { case ((accList, prevOffset), offsetDef) =>
      val comp = offsetDef.getComputedOffset(prevOffset)
      (accList :+ comp, comp)
    }
    built.toArray
  }

  /** @inheritdoc */
  override protected val name: String = "UTC"

  /** @inheritdoc */
  override def timePastTAI(time: AbsoluteTime): TimeFormat =
    findOffset(time) match {
      case Some(offset) => offset.offsetFrom(time).negate() // we have to negate since UTC is behind TAI
      case None => TimeFormat.Zero // The time is before the implementation of leap seconds, so the offset between UTC and TAI would be Zero.
    }

  /** Finds the correct offset from computed leap second offsets */
  private def findOffset(time: AbsoluteTime): Option[ComputedLeapSecondOffset] =
    computedLeapOffsets.findLast(_.startTime.compareTo(time) < 0)

  /** Finds the index of the correct offset from computed leap second offsets */
  private def findOffsetIndex(time: AbsoluteTime): Int =
    computedLeapOffsets.lastIndexWhere(_.startTime.compareTo(time) < 0)

  /** Finds the correct offset from computed leap second offsets */
  private def findOffset(mjd: Int): Option[ComputedLeapSecondOffset] = {
    val idx = computedLeapOffsets.indexWhere(_.mjdDate > mjd)
    if (idx == -1) computedLeapOffsets.lastOption
    else if (idx == 0) None
    else Some(computedLeapOffsets(idx - 1))
  }

  /** @inheritdoc */
  override def isInsideLeapSecond(time: AbsoluteTime): Boolean =
    findOffset(time) match {
      case Some(offset) => time.compareTo(offset.validityStart) < 0
      case None => false
    }

  /** @inheritdoc */
  override def minuteDuration(time: AbsoluteTime): Int =
    findOffsetIndex(time: @switch) match {
      // Case 1: No applicable leap second offset found
      case -1 => 60 // Return standard 60-second minute

      // Case 2: Found a potential leap second offset
      case i =>
        // Determine the correct leap second offset to apply based on timing conditions
        val offset: ComputedLeapSecondOffset = {
          // Check if the time falls before the offset's validity start
          if time.compareTo(computedLeapOffsets(i).validityStart) < 0 then
            computedLeapOffsets(i) // Use current offset

          // Check if there's a next offset and if we're within 60 seconds of its start time
          else if i + 1 < computedLeapOffsets.length &&
            computedLeapOffsets(i + 1).startTime.durationFrom(time).toDouble <= 60.0 then
            computedLeapOffsets(i) // Still use current offset (we're in the transition period)

          // Time falls outside leap second adjustment periods
          else return 60 // Return standard minute duration
        }

        // Calculate the adjusted minute duration
        // Base 60 seconds + leap second adjustment (seconds + fractional part capped at 1)
        60 + (offset.leap.getSeconds + math.min(1, offset.leap.getAttoSeconds)).toInt
    }

  /** @inheritdoc */
  override def timeToTAI(date: Date, time: Time): TimeFormat =
    val min = time.getHour * 60 + time.getMinute - time.getUtcOffset
    val c = if min > 60 then (min - 1439) / 1440 else min / 1440

    val mjd = date.getMJD + c

    findOffset(mjd) match {
      case Some(offset) => offset.offsetFrom(date, time)
      case None => TimeFormat.Zero
    }

  /** @inheritdoc */
  override def getLeap(time: AbsoluteTime): TimeFormat =
    findOffset(time) match {
      case Some(offset) => offset.leap
      case None => TimeFormat.Zero
    }
}

object UTCScale {
  /**
   * Used to represent leap second offset.
   *
   * @param startDate   The start time of the leap second
   * @param mjdBase     The mjd base time for the calculation of leap
   * @param fixedOffset The fixed offset for leap second calculation
   * @param rate        The rate for the calculation of the leap
   */
  case class LeapSecondOffset(startDate: Date, mjdBase: Int, fixedOffset: Double, rate: Double)(implicit tai: TimeScale) {

    import breeze.numerics.abs

    def getComputedOffset(previousOffset: ComputedLeapSecondOffset): ComputedLeapSecondOffset = {
      val fixedOffsetTime = TimeFormat.fromDouble(fixedOffset)
      val prev = if previousOffset == null then TimeFormat.Zero else previousOffset.offsetFrom(startDate, Time(0, 0, TimeFormat.Zero))
      val r = getRateInNanosPerSec(rate)

      val dt = (startDate.getMJD - mjdBase) * SECONDS_IN_DAY
      val drift = TimeFormat.NANOSECOND * (abs(dt) * r)
      val startOffset = if dt < 0 then TimeFormat.fromDouble(fixedOffset) - drift else TimeFormat.fromDouble(fixedOffset) + drift

      val leapStart = new AbsoluteTime(startDate, tai) ++ prev
      val leapEnd = new AbsoluteTime(startDate, tai) ++ startOffset
      val leap = leapEnd.durationFrom(leapStart).*(NANOS_IN_SECONDS)./(r + NANOS_IN_SECONDS)

      val mjdBaseTime = AbsoluteTime.fromMJDDate(mjdBase, tai) ++ fixedOffsetTime


      ComputedLeapSecondOffset(
        leapStart,
        startDate.getMJD,
        mjdBaseTime,
        mjdBase,
        fixedOffsetTime,
        r,
        leap
      )
    }
  }

  /**
   * Represents a computed leap second offset, effective from a specific start time.
   *
   * @param startTime   The time when this leap second offset becomes effective.
   * @param mjdDate     leap date in Modified Julian Day
   * @param mjdBaseTime The base time as an AbsoluteTime, typically corresponding to a Modified Julian Date (MJD) reference.
   * @param fixedOffset The fixed time offset (in the specified TimeFormat) to apply from the base time.
   * @param rate        The rate of change of the offset, usually in units per day (e.g., nanoseconds per second per day).
   * @param leap        value of the leap at offset validity start
   */
  case class ComputedLeapSecondOffset(startTime: AbsoluteTime, mjdDate: Int, mjdBaseTime: AbsoluteTime, mjdBase: Int, fixedOffset: TimeFormat, rate: Int, leap: TimeFormat) {
    val validityStart: AbsoluteTime = startTime ++ leap

    def offsetFrom(time: AbsoluteTime): TimeFormat =
      if rate == 0 then return fixedOffset

      val dt = time.durationFrom(mjdBaseTime)

      val drift = dt.*(rate)./(rate + NANOS_IN_SECONDS)

      fixedOffset + drift

    def offsetFrom(date: Date, time: Time): TimeFormat = {
      if rate == 0 then return fixedOffset

      val dt = TimeFormat((date.getMJD - mjdBase) * SECONDS_IN_DAY + time.getHour * 3600 + time.getMinute * 60 + time.getSecondsAsTimeFormat.getSeconds, time.getSecondsAsTimeFormat.getAttoSeconds)

      val drift = dt.*(rate)./(NANOS_IN_SECONDS)

      fixedOffset + drift
    }
  }

  /**
   * Converts the seconds per day rate in the UTC-TAI.history file to nanoseconds per seconds
   *
   * @param rate The rate of leap offset, in s/day
   * @return The rate in ns/s
   */
  private def getRateInNanosPerSec(rate: Double): Int =
    ((rate * NANOS_IN_SECONDS) / SECONDS_IN_DAY).toInt
}