package org.abh80.nf
package core.time

import breeze.numerics.abs

import java.time.{Instant, ZoneOffset, ZonedDateTime}
import scala.util.hashing.MurmurHash3

private val SECONDS_IN_HOUR = 3600
private val SECONDS_IN_MINUTE = 60

/**
 * A robust time implementation with Hour, Minutes, Seconds of a day. Seconds are in precision up to attoseconds.
 *
 * @note This class is immutable and thread-safe
 * @see TimeFormat
 * @see Date
 */
class Time private extends Comparable[Time] with Serializable {
  /** The hour of the day */
  private var hour: Int = _

  /** The minute of the hour */
  private var minute: Int = _

  /** The seconds of the minute
   *
   * @see TimeFormat
   */
  private var seconds: TimeFormat = _

  /** The UTC offset +ve or -ve in minutes */
  private var utcOffset: Int = _

  /**
   * Build a time object from clock elements.
   *
   * @param hour      the hour of the day
   * @param minute    the minute of the hour
   * @param seconds   the second of the minute
   * @param utcOffset the offset from utc represented in minutes, in +ve or -ve
   * @throws IllegalArgumentException when invalid time components are provided, example: 24 hours
   */
  @throws[IllegalArgumentException]
  def this(hour: Int, minute: Int, seconds: TimeFormat, utcOffset: Int) = {
    this()
    require(hour >= 0 && hour < 24 && minute >= 0 && minute < 60 && seconds.getSeconds >= 0 && seconds.getSeconds <= 61)

    this.hour = hour
    this.minute = minute
    this.seconds = seconds
    this.utcOffset = utcOffset
  }

  /**
   * Build a time object from clock elements.
   *
   * @param hour      the hour of the day
   * @param minute    the minute of the hour
   * @param seconds   the second of the minute
   * @param utcOffset the offset from utc represented in minutes, in +ve or -ve
   * @throws IllegalArgumentException when invalid time components are provided, example: 24 hours
   */
  @throws[IllegalArgumentException]
  def this(hour: Int, minute: Int, seconds: Double, utcOffset: Int) =
    this(hour, minute, TimeFormat.fromDouble(seconds), utcOffset)

  /**
   * Build a time object from clock elements.
   *
   * @param hour    the hour of the day
   * @param minute  the minute of the hour
   * @param seconds the second of the minute
   * @throws IllegalArgumentException when invalid time components are provided, example: 24 hours
   */
  @throws[IllegalArgumentException]
  def this(hour: Int, minute: Int, seconds: TimeFormat) =
    this(hour, minute, seconds, 0)

  /**
   * Build a time object from clock elements.
   *
   * @param hour    the hour of the day
   * @param minute  the minute of the hour
   * @param seconds the second of the minute
   * @throws IllegalArgumentException when invalid time components are provided, example: 24 hours
   */
  @throws[IllegalArgumentException]
  def this(hour: Int, minute: Int, seconds: Double) =
    this(hour, minute, TimeFormat.fromDouble(seconds))

  /**
   * Build a time object from an `Instant`.
   *
   * @param instant the `Instant` to convert to a `Time` object
   * @throws IllegalArgumentException if the conversion fails
   *
   *                                  This constructor assumes the UTC time zone and extracts the hour, minute, 
   *                                  and second components from the provided `Instant`. The UTC offset is set to 0.
   */
  @throws[IllegalArgumentException]
  def this(instant: Instant) = {
    this()
    val zonedDateTime: ZonedDateTime = instant.atZone(ZoneOffset.UTC)
    this.hour = zonedDateTime.getHour
    this.minute = zonedDateTime.getMinute
    this.seconds = TimeFormat.fromDouble(zonedDateTime.getSecond + zonedDateTime.getNano / 1e9)
    this.utcOffset = 0 // UTC offset is 0 for UTC
  }

  /**
   * Build a time object from seconds in a day.
   *
   * @param seconds the seconds parsed in TimeFormat
   * @throws IllegalArgumentException when TimeFormat cannot parse the seconds
   * @see TimeFormat
   * @see #Time(Double)
   */
  @throws[IllegalArgumentException]
  def this(seconds: TimeFormat) = {
    this()
    require(seconds.compareTo(TimeFormat.Zero) >= 0, "Seconds cannot be negative")
    if seconds.compareTo(TimeFormat.DAY) >= 0 then {
      require(seconds.compareTo(TimeFormat(86401L, 0L)) < 0, "Invalid leap second")
      hour = 23
      minute = 59
      this.seconds = seconds.-(TimeFormat(23 * SECONDS_IN_HOUR + 59 * SECONDS_IN_MINUTE, 0L))
    } else {
      hour = (seconds.getSeconds / SECONDS_IN_HOUR).toInt
      minute = ((seconds.getSeconds % SECONDS_IN_HOUR) / SECONDS_IN_MINUTE).toInt
      this.seconds = seconds.-(TimeFormat(hour * SECONDS_IN_HOUR + minute * SECONDS_IN_MINUTE, 0L))
    }
  }

  /**
   * Build a Time object from seconds of a local day.
   *
   * @param seconds the seconds in double
   * @throws IllegalArgumentException when TimeFormat cannot parse the seconds
   * @see TimeFormat#fromDouble
   */
  @throws[IllegalArgumentException]
  def this(seconds: Double) =
    this(TimeFormat.fromDouble(seconds))

  def this(seconds: TimeFormat, leap: TimeFormat, minuteDuration: Int) = {
    this()
    require(seconds.compareTo(TimeFormat.Zero) >= 0 && seconds.compareTo(TimeFormat.DAY) < 0)

    val remainingSeconds = minuteDuration - 60
    require(leap.getSeconds * remainingSeconds >= 0)
    require(abs(leap.getSeconds) <= abs(remainingSeconds))

    var roundedSeconds = seconds.getSeconds.toInt
    hour = roundedSeconds / SECONDS_IN_HOUR
    roundedSeconds -= hour * SECONDS_IN_HOUR
    minute = roundedSeconds / SECONDS_IN_MINUTE
    roundedSeconds -= minute * SECONDS_IN_MINUTE

    val secondsInMinute = TimeFormat(roundedSeconds, seconds.getAttoSeconds) + leap

    require(secondsInMinute.compareTo(TimeFormat.Zero) >= 0)

    if secondsInMinute.getSeconds < minuteDuration then this.seconds = secondsInMinute
    else this.seconds = TimeFormat(minuteDuration - 1, 999_999_999_999_999_999L)
  }

  /** Get the hour of the day */
  def getHour: Int = hour

  /** Get the minute of the hour */
  def getMinute: Int = minute

  /** Get the seconds of the minute */
  def getSeconds: Double = seconds.toDouble

  /** Get the seconds of the minute as time format */
  def getSecondsAsTimeFormat: TimeFormat = seconds

  /** Get the utc offset in minutes */
  def getUtcOffset: Int = utcOffset

  /** Compare with another Time Object */
  override def compareTo(o: Time): Int = getSecondsInUTCDay.compareTo(o.getSecondsInUTCDay)

  /** Get the seconds of the day but adjusted with utc offset */
  def getSecondsInUTCDay: Double = getSecondsInDayObject(utcOffset).toDouble

  /** Get the seconds of the day */
  def getSecondsInDay: Double = getSecondsInDayObject().toDouble

  /** Returns the seconds in a day as TimeFormat */
  def getSecondsInDayObject(offset: Int = 0) = TimeFormat(hour * SECONDS_IN_HOUR + (minute - offset) * SECONDS_IN_MINUTE, 0L).+(seconds)

  /** Check equality */
  override def equals(obj: Any): Boolean = obj match {
    case t: Time => t.hour == hour && t.minute == minute && t.seconds.equals(seconds) && t.utcOffset == utcOffset
    case _ => false
  }

  /** Hashcode */
  override def hashCode(): Int = MurmurHash3.productHash((hour, minute - utcOffset, seconds.hashCode()))

  /** Get the current time formatted in IS08601 UTC */
  override def toString: String = trimTrailingZerosFromISOString(toISO8601String()) + getUTCOffsetString

  /** Get the current time formatted in IS08601; no UTC offset is added */
  def toISO8601String(secondsPrecision: Int = 18): String = {
    val rounded = seconds.getRoundedFormat(secondsPrecision)

    if (secondsPrecision > 0) {
      f"$hour%02d:$minute%02d:${rounded.getSeconds}%02d." ++ String.format(s"%0${secondsPrecision}d", rounded.getAttoSeconds)
    } else {
      f"$hour%02d:$minute%02d:${rounded.getSeconds}%02d"
    }
  }
  
  def toISO8601StringTrimmed(secondsPrecision: Int = 18) : String =
    trimTrailingZerosFromISOString(toISO8601String(secondsPrecision))

  /** Get the current UTC offset in ISO8601 standard */
  def getUTCOffsetString: String = {
    val hourOffset = abs(utcOffset / 60)
    val minuteOffset = abs(utcOffset % 60)
    val sign = if (utcOffset < 0) "-" else "+"

    f"$sign$hourOffset%02d:$minuteOffset%02d"
  }

  /** Keeps precision of zeros till milliseconds if the remaining portion is 0 */
  private def trimTrailingZerosFromISOString(str: String): String =
    var last = str.length - 1

    while (last > 11 && str.charAt(last) == '0') last = last - 1

    str.substring(0, last + 1)
}

object Time {
  /** Get the current instant as a `Time` object */
  def now: Time = new Time(Instant.now())
  
  /** 00:00:00 as Time */
  val MIDNIGHT = Time(0 , 0 , TimeFormat.Zero)
  
  /** 12:00:00 as Time */
  val AFTERNOON = Time(12, 0, TimeFormat.Zero)
}