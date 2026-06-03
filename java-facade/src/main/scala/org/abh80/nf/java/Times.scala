package org.abh80.nf.java

import org.abh80.nf.core.time.{AbsoluteTime, Date, Time, TimeFormat, TimeScale}

import java.time.Instant

/**
 * Java-idiomatic facade for [[org.abh80.nf.core.time.AbsoluteTime]] construction and shifting.
 *
 * The Scala API exposes 8+ constructors and the `++` operator. From Java these are awkward:
 * `new AbsoluteTime(...)` works but operators (`++`) require calling mangled names. This object
 * provides named factories and a `shiftedBy` method.
 */
object Times {

  /** Default epoch — J2000 (2000-01-01 12:00 TT). */
  def epoch: AbsoluteTime = new AbsoluteTime()

  /** Build from a calendar date, time-of-day, and time scale. */
  def of(date: Date, time: Time, scale: TimeScale): AbsoluteTime =
    new AbsoluteTime(date, time, scale)

  /** Build from year/month/day/hour/minute/seconds (Double) in the given scale. */
  def of(year: Int, month: Int, day: Int, hour: Int, minute: Int, seconds: Double, scale: TimeScale): AbsoluteTime =
    new AbsoluteTime(year, month, day, hour, minute, seconds, scale)

  /** Build from year/month/day/hour/minute/seconds (attosecond precision) in the given scale. */
  def of(year: Int, month: Int, day: Int, hour: Int, minute: Int, seconds: TimeFormat, scale: TimeScale): AbsoluteTime =
    new AbsoluteTime(year, month, day, hour, minute, seconds, scale)

  /** Build from a date + scale (midnight). */
  def of(date: Date, scale: TimeScale): AbsoluteTime = new AbsoluteTime(date, scale)

  /** Build from year/month/day + scale (midnight). */
  def of(year: Int, month: Int, day: Int, scale: TimeScale): AbsoluteTime =
    new AbsoluteTime(year, month, day, scale)

  /** Build from a Java [[java.time.Instant]] in the given scale. */
  def fromInstant(instant: Instant, scale: TimeScale): AbsoluteTime =
    new AbsoluteTime(instant, scale)

  /** Build from a Java [[java.time.Instant]] using UTC. */
  def fromInstant(instant: Instant): AbsoluteTime = new AbsoluteTime(instant)

  /** Current system time as an AbsoluteTime (UTC). */
  def now(): AbsoluteTime = AbsoluteTime.now()

  /** Shift by `dt` seconds. */
  def shiftedBy(t: AbsoluteTime, dt: Double): AbsoluteTime = t ++ dt

  /** Shift by a TimeFormat duration. */
  def shiftedBy(t: AbsoluteTime, dt: TimeFormat): AbsoluteTime = t ++ dt

  /** Duration `later - earlier` as a TimeFormat. */
  def durationBetween(later: AbsoluteTime, earlier: AbsoluteTime): TimeFormat =
    later.durationFrom(earlier)
}
