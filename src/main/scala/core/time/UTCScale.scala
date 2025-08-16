package org.abh80.nf
package core.time

private val NANOS_IN_SECONDS = 1_000_000_000
private val SECONDS_IN_DAY = 86400

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
      LeapSecondOffset(Date(1972, 1, 1), 0, 10, 0),
      LeapSecondOffset(Date(1972, 7, 1), 0, 11, 0),
      LeapSecondOffset(Date(1973, 1, 1), 0, 12, 0),
      LeapSecondOffset(Date(1974, 1, 1), 0, 13, 0),
      LeapSecondOffset(Date(1975, 1, 1), 0, 14, 0),
      LeapSecondOffset(Date(1976, 1, 1), 0, 15, 0),
      LeapSecondOffset(Date(1977, 1, 1), 0, 16, 0),
      LeapSecondOffset(Date(1978, 1, 1), 0, 17, 0),
      LeapSecondOffset(Date(1979, 1, 1), 0, 18, 0),
      LeapSecondOffset(Date(1980, 1, 1), 0, 19, 0),
      LeapSecondOffset(Date(1981, 7, 1), 0, 20, 0),
      LeapSecondOffset(Date(1982, 7, 1), 0, 21, 0),
      LeapSecondOffset(Date(1983, 7, 1), 0, 22, 0),
      LeapSecondOffset(Date(1985, 7, 1), 0, 23, 0),
      LeapSecondOffset(Date(1988, 1, 1), 0, 24, 0),
      LeapSecondOffset(Date(1990, 1, 1), 0, 25, 0),
      LeapSecondOffset(Date(1991, 1, 1), 0, 26, 0),
      LeapSecondOffset(Date(1992, 7, 1), 0, 27, 0),
      LeapSecondOffset(Date(1993, 7, 1), 0, 28, 0),
      LeapSecondOffset(Date(1994, 7, 1), 0, 29, 0),
      LeapSecondOffset(Date(1996, 1, 1), 0, 30, 0),
      LeapSecondOffset(Date(1997, 7, 1), 0, 31, 0),
      LeapSecondOffset(Date(1999, 1, 1), 0, 32, 0),
      LeapSecondOffset(Date(2006, 1, 1), 0, 33, 0),
      LeapSecondOffset(Date(2009, 1, 1), 0, 34, 0),
      LeapSecondOffset(Date(2012, 7, 1), 0, 35, 0),
      LeapSecondOffset(Date(2015, 7, 1), 0, 36, 0),
      LeapSecondOffset(Date(2017, 1, 1), 0, 37, 0)
    ) ++ leapOffsets
  }
  
  /** Adjusted to previous offsets with respect to TAI */
  private val computedLeapOffsets: Array[ComputedLeapSecondOffset] = {
    val (built, _) = fixedLeapOffsets.foldLeft(
      (List.empty[ComputedLeapSecondOffset], TimeFormat.Zero)
    ) { case ((accList, prevOffset), offsetDef) =>
      val comp = offsetDef.getComputedOffset(prevOffset)
      val nextPrev = comp.offsetFrom(comp.startTime)
      (accList :+ comp, nextPrev)
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


  /** @inheritdoc */
  override def isInsideLeapSecond(time: AbsoluteTime): Boolean =
    findOffset(time) match {
      case Some(offset) => time.compareTo(offset.startTime) < 0
      case None => false
    }

  /** @inheritdoc */
  override def minuteDuration(time: AbsoluteTime): Int =
    ???
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
    def getComputedOffset(previousOffset: TimeFormat): ComputedLeapSecondOffset =
      ComputedLeapSecondOffset(
        AbsoluteTime(startDate, tai) ++ previousOffset,
        AbsoluteTime.fromMJDDate(mjdBase, tai), TimeFormat.fromDouble(fixedOffset),
        getRateInNanosPerSec(rate)
      )
  }

  /**
   * Represents a computed leap second offset, effective from a specific start time.
   *
   * @param startTime   The time when this leap second offset becomes effective.
   * @param mjdBaseTime The base time as an AbsoluteTime, typically corresponding to a Modified Julian Date (MJD) reference.
   * @param fixedOffset The fixed time offset (in the specified TimeFormat) to apply from the base time.
   * @param rate        The rate of change of the offset, usually in units per day (e.g., nanoseconds per second per day).
   */
  case class ComputedLeapSecondOffset(startTime: AbsoluteTime, mjdBaseTime: AbsoluteTime, fixedOffset: TimeFormat, rate: Int) {
    def offsetFrom(time: AbsoluteTime): TimeFormat =
      if rate == 0 then return fixedOffset

      val dt = time.durationFrom(mjdBaseTime)

      val drift = dt.*(rate)./(rate + NANOS_IN_SECONDS)

      fixedOffset + drift
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