package org.abh80.nf
package core.time

import util.DateUtil.{ISO8601, Month, StringDateFormat}

import java.time.Instant
import scala.util.hashing.MurmurHash3

private val JULIAN_DAY_AT_J2000 = 2451545
private val JULIAN_YEAR_START_J2000 = -730122
private val J2000_TO_MJD_FACTOR = 51544


/** A robust date implementation that handles dates in different calendar systems.
 *
 * This class provides functionality to work with dates across three calendar systems:
 * - Gregorian calendar (current standard calendar system)
 * - Julian calendar (historical calendar used before Gregorian)
 * - Proleptic Julian calendar (extension of Julian calendar for dates before year 1)
 *
 * The implementation uses J2000 day number as internal representation, which counts
 * days from January 1, 2000 (known as the J2000 epoch in astronomy).
 *
 * The class automatically handles calendar transitions:
 * - Before year 1: Proleptic Julian calendar
 * - Years 1-1582: Julian calendar
 * - After October 4, 1582: Gregorian calendar
 *
 * @note This class is immutable and thread-safe
 * @note it's recommended to initialize this class from the `Date` companion object
 */
class Date private extends Comparable[Date] with Serializable {
  // Day of the month, in ISO standards, 1 till 31
  private var day: Int = -1

  // Month of the year in ISO standards, 1 till 12
  private var month: Int = -1

  // Year in ISO standards, positive for A.D and negetive for B.C
  private var year: Int = -1

  /** Returns the ISO week number for the date.
   *
   * The ISO week numbering system defines:
   * - Week 1 is the week containing January 4th
   * - Weeks start on Monday
   * - Week can belong to different year than the date
   *
   * @return Week number (1-53)
   */
  def getWeek: Int =
    val week1Monday = getWeek1MondayOfYear(year)
    var daysSinceWeek1Monday = getJ2000Day - week1Monday

    if daysSinceWeek1Monday < 0 then {
      daysSinceWeek1Monday += week1Monday - getWeek1MondayOfYear(year - 1)
    } else if daysSinceWeek1Monday > 363 then
      val weekLengthOfYear = getWeek1MondayOfYear(year + 1) - week1Monday

      if daysSinceWeek1Monday >= weekLengthOfYear then daysSinceWeek1Monday -= weekLengthOfYear

    1 + daysSinceWeek1Monday / 7

  /** Constructs a Date from year, month, and day components.
   *
   * @param year  The year (can be negative for BCE dates)
   * @param month The month (1-12)
   * @param day   The day of month
   * @throws IllegalArgumentException if the date components form an invalid date
   */
  @throws[IllegalArgumentException]
  def this(year: Int, month: Int, day: Int) = {
    this()

    this.day = day
    this.month = month
    this.year = year

    val check = Date(getJ2000Day)
    if check.year != year || month != check.month || year != check.year then throw new IllegalArgumentException("The provided year/month/day sequence does not exist")
  }


  /** Constructs a Date from J2000 day offset.
   *
   * @param j2000DayOffset Number of days since January 1, 2000
   */
  def this(j2000DayOffset: Int) = {
    this()
    val yf = getYearFactory(j2000DayOffset)

    year = yf.getYear(j2000DayOffset)

    val days = j2000DayOffset - yf.getLastJ2000DayOfYear(year - 1)
    val mf = getMonthFactory(yf, year)

    month = mf.getMonth(days)
    day = mf.getDayOfMonth(days, month)
  }

  private def getYearFactory(j2000DayOffset: Int): YearFactory = {
    var yf: YearFactory = GeorgianYear
    if j2000DayOffset < -152384 then
      if j2000DayOffset > JULIAN_YEAR_START_J2000 then yf = JulianYear
      else yf = ProlepticJulianYear
    yf
  }

  /**
   * Constructs a Date from java Instant
   */
  def this(instant: Instant) =
    this(((instant.getEpochSecond + instant.getNano / 1e9) / 86400).toInt - 10957)

  /** Returns the Julian Day Number for this date.
   *
   * The Julian Day Number is a continuous count of days since the beginning
   * of the Julian Period, used primarily in astronomy.
   *
   * @return Julian Day Number
   */
  def getJulianDay: Int =
    JULIAN_DAY_AT_J2000 + getJ2000Day

  /** Returns the month number (1-12).
   *
   * @return Month as integer where 1=January, 12=December
   */
  def getMonth: Int = month

  /** Returns the month as an enumerated type.
   *
   * @return Month enum value
   */
  def getMonthAsEnum: Month = Month.getMonth(month)

  /** Returns the day of month (1-31).
   *
   * @return Day of month
   */
  def getDay: Int = day

  /** Returns the ISO day of week.
   *
   * In ISO standard:
   * - Monday = 1
   * - Tuesday = 2
   * ...
   * - Sunday = 7
   *
   * @return Day of week (1-7)
   */
  def getDayOfWeek: Int =
    val abs = (getJ2000Day + 6) % 7
    if abs < 1 then abs + 7 else abs // abs will be 0 when it's Sunday, but Sunday is considered the 7th day of a week

  /** Returns the day of year (1-366).
   *
   * January 1st is day 1, December 31st is day 365/366.
   *
   * @return Day of year
   */
  def getDayOfYear: Int =
    getJ2000Day - Date(year - 1, 12, 31).getJ2000Day

  /** Returns the J2000 day number for this date.
   *
   * The J2000 day number is the number of days since January 1, 2000.
   * Negative values indicate dates before J2000.
   *
   * @return J2000 day number
   */
  def getJ2000Day: Int =
    var yf: YearFactory = GeorgianYear
    if year <= 1582 then
      if year < 1 then yf = ProlepticJulianYear
      else if year < 1582 || month < 10 || month <= 10 && day <= 4 then yf = JulianYear

    val mf = getMonthFactory(yf, year)
    yf.getLastJ2000DayOfYear(year - 1) + mf.getDay(day, month)

  private def getMonthFactory(yearFactory: YearFactory, year: Int): MonthFactory = if yearFactory.isLeap(year) then LeapYearFactory else NonLeapYearFactory

  /** Returns the year component.
   *
   * @return Year (negative values indicate BCE)
   */
  def getYear: Int = year

  override def toString: String =
    toString(ISO8601.apply)

  /** Converts the date to a string representation using the specified format.
   *
   * @param f The format function that defines the output format
   * @return Formatted date string
   */
  def toString(f: (Int, Int, Int) => StringDateFormat): String =
    f(day, month, year).toString

  override def compareTo(o: Date): Int = getJ2000Day.compareTo(o.getJ2000Day)

  /** Get the modified julian day */
  def getMJD: Int = J2000_TO_MJD_FACTOR + getJ2000Day

  @SuppressWarnings(Array("NonValueFieldInHashCode"))
  override def hashCode(): Int =
    MurmurHash3.productHash((year, month, day))

  override def equals(obj: Any): Boolean = obj match {
    case other: Date => this.year == other.year && this.month == other.month && this.day == other.day
    case _ => false
  }

  /** 4th of January always lie on the first week of any year
   * Source: https://en.wikipedia.org/wiki/ISO_week_date
   */
  private def getWeek1MondayOfYear(year: Int): Int =

    val jan4 = new Date(year, 1, 4)
    val jan4DayOfWeek = jan4.getDayOfWeek

    jan4.getJ2000Day - jan4DayOfWeek + 1

  private sealed trait YearFactory {
    def getYear(j2000Day: Int): Int

    def getLastJ2000DayOfYear(year: Int): Int

    def isLeap(year: Int): Boolean
  }

  private sealed trait MonthFactory {
    val PREVIOUS_MONTH_END_DAYS: Array[Int]

    def getMonth(days: Int): Int = PREVIOUS_MONTH_END_DAYS.lastIndexWhere(_ < days)

    def getDayOfMonth(days: Int, month: Int): Int = days - PREVIOUS_MONTH_END_DAYS(month)

    def getDay(day: Int, month: Int): Int = day + PREVIOUS_MONTH_END_DAYS(month)
  }

  private object JulianYear extends YearFactory {
    override def getYear(j2000Day: Int): Int =
      val f_years = math.floor(j2000Day / 365.25)
      2000 + f_years.toInt

    override def getLastJ2000DayOfYear(year: Int): Int = {
      365 * year + year / 4 + JULIAN_YEAR_START_J2000
    }


    override def isLeap(year: Int): Boolean =
      year % 4 == 0
  }

  private object ProlepticJulianYear extends YearFactory {
    override def getYear(j2000Day: Int): Int =
      // credits: https://gist.github.com/PM2Ring/f078d8d60b7203e423b229fc4afa3a04
      val jdn = j2000Day + 2451545

      val A = jdn + 32082

      val B = (4 * A + 3) / 1461
      val C = A - (1461 * B) / 4
      val D = (5 * C + 2) / 153

      val year = B - 4800 + ((D + 2) / 12)
      year

    // credits: Meeus's Astronomical Algorithms
    override def getLastJ2000DayOfYear(year: Int): Int =
      val dayOfYear = if (isLeap(year)) 366 else 365
      val jdn = 1721423 + 365 * (year - 1) + (year - 1) / 4 + dayOfYear - 1
      jdn - JULIAN_DAY_AT_J2000

    override def isLeap(year: Int): Boolean =
      year % 4 == 0
  }

  private object GeorgianYear extends YearFactory {
    // Edward Graham Richards algorithm, only valid for j2000 day > 0, after 2000. Proceed with caution
    override def getYear(j2000Day: Int): Int = {
      val JD = JULIAN_DAY_AT_J2000.toLong + j2000Day

      val y = 4716
      val j = 1401
      val m = 2
      val n = 12
      val r = 4
      val p = 1461
      val v = 3
      val u = 5
      val s = 153
      val w = 2
      val B = 274277
      val C = -38

      val f = JD + j + (((4 * JD + B) / 146097) * 3) / 4 + C
      val e = r * f + v
      val g = (e % p) / r
      val h = u * g + w
      val M = ((h / s) + m) % n + 1
      val Y = (e / p) - y + (n + m - M) / n

      Y.toInt
    }


    override def getLastJ2000DayOfYear(year: Int): Int =
      var y = year
      var m = 12
      val d = 31

      if (m <= 2) {
        y -= 1
        m += 12
      }

      val A = y / 100
      val B = 2 - A + (A / 4)

      val jd = (365.25 * (y + 4716)).toInt +
        (30.6001 * (m + 1)).toInt +
        d + B - 1524

      val j2000Day = jd - JULIAN_DAY_AT_J2000
      j2000Day

    override def isLeap(year: Int): Boolean = ((year % 4) eq 0) && (((year % 400) eq 0) || ((year % 100) ne 0))
  }

  private object LeapYearFactory extends MonthFactory {
    override val PREVIOUS_MONTH_END_DAYS: Array[Int] = Array(-10000, 0, 31, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335)
  }

  private object NonLeapYearFactory extends MonthFactory {
    override val PREVIOUS_MONTH_END_DAYS: Array[Int] = Array(-10000, 0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334)
  }
}

/** Factory methods for creating Date instances.
 *
 * This companion object provides various ways to construct Date objects
 * using different date components and epoch references.
 */

/**
 * Factory methods for creating `Date` instances.
 *
 * This companion object provides various `apply` methods to construct `Date` objects
 * using different date components and epoch references. These methods offer flexibility
 * in creating `Date` instances from J2000 offsets, year/month/day combinations,
 * or year/day-of-year combinations.
 */
object Date {

  /** The J2000.0 epoch, which is the fundamental epoch for the ICRF reference frame.
   * Defined as January 1, 2000, at 12:00 TT (Terrestrial Time).
   */
  val J2000_0: Date = Date(2000, 1, 1)
  /** The Julian epoch, which marks the beginning of the Julian calendar.
   * Defined as January 1, 4713, BCE at 12:00 UT.
   */
  val JULIAN: Date = Date(-4712, 1, 1)
  /** The Unix epoch, used as the starting point for Unix timestamps.
   * Defined as January 1, 1970, at 00:00:00 UTC.
   */
  val UNIX: Date = Date(1970, 1, 1)
  /** The CXCSEC epoch, used by the Chandra X-ray Observatory.
   * Defined as January 1, 1998, at 00:00:00 TT.
   */
  val CXCSEC: Date = Date(1998, 1, 1)
  /** The GPS epoch, used by the Global Positioning System.
   * Defined as January 6, 1980, at 00:00:00 UTC.
   */
  val GPS: Date = Date(1980, 1, 6)

  /** The modified julian date, which started at midnight on November 17, 1858 (Gregorian calendar).*/
  val MJD: Date = Date(1858, 11, 17)

  /**
   * Constructs a `Date` instance from a J2000 day offset.
   *
   * This method allows creating a `Date` object directly from the number of days
   * since the J2000 epoch (January 1, 2000).
   *
   * @param j2000Offset The number of days since January 1, 2000.
   * @return A new `Date` instance representing the specified J2000 offset.
   * @example
   * {{{
   *   val epoch: Date = Date(0) // January 1, 2000
   *   val futureDate: Date = Date(365) // January 1, 2001
   *   val pastDate: Date = Date(-365) // January 1, 1999
   * }}}
   */
  def apply(j2000Offset: Int): Date =
    new Date(j2000Offset)

  /**
   * Constructs a `Date` instance from year, month (as a `Month` enum), and day components.
   *
   * This method facilitates creating a `Date` object using a year, a `Month` enum value,
   * and a day of the month. It internally converts the `Month` enum to its integer representation.
   *
   * @param year  The year of the date.
   * @param month The month of the year as a `Month` enum.
   * @param day   The day of the month.
   * @return A new `Date` instance representing the specified year, month, and day.
   * @example
   * {{{
   *   import core.time.util.DateUtil.Month
   *
   *   val christmas: Date = Date(2024, Month.December, 25)
   * }}}
   */
  def apply(year: Int, month: Month, day: Int): Date =
    new Date(year, month.getIntegerValue, day)

  /**
   * Constructs a `Date` instance from a year and the day of the year.
   *
   * This method creates a `Date` object from a given year and the day of the year
   * (where January 1st is day 1). It calculates the corresponding date by adding
   * the specified number of days to the last day of the previous year.
   *
   * @param year The year of the date.
   * @param days The day of the year (1-366).
   * @return A new `Date` instance representing the specified year and day of the year.
   * @example
   * {{{
   *   val newYearsDay: Date = Date(2024, 1) // January 1, 2024
   *   val lastDayOfYear: Date = Date(2024, 366) // December 31, 2024 (if leap year)
   * }}}
   */
  def apply(year: Int, days: Int): Date =
    Date(J2000_0, Date(year - 1, 12, 31).getJ2000Day + days)

  /**
   * Constructs a `Date` instance from year, month, and day components.
   *
   * This method is the primary way to create a `Date` object from its individual
   * components: year, month, and day.
   *
   * @param year  The year of the date.
   * @param month The month of the year (1-12).
   * @param day   The day of the month.
   * @return A new `Date` instance representing the specified year, month, and day.
   * @throws IllegalArgumentException if the date components form an invalid date
   * @example
   * {{{
   *   val independenceDay: Date = Date(1776, 7, 4) // July 4, 1776
   * }}}
   */
  def apply(year: Int, month: Int, day: Int): Date =
    new Date(year, month, day)

  /**
   * Constructs a `Date` instance from an epoch `Date` and a J2000 day offset.
   *
   * This method creates a new `Date` object relative to a specified epoch `Date`
   * by adding a J2000 day offset to the epoch's J2000 day number.
   *
   * @param epoch       The base `Date` to use as the epoch.
   * @param j2000Offset The number of days to offset from the epoch.
   * @return A new `Date` instance representing the date at the specified offset from the epoch.
   * @example
   * {{{
   *   val epoch: Date = Date(2020, 1, 1)
   *   val futureDate: Date = Date(epoch, 100) // 100 days after January 1, 2020
   * }}}
   */
  def apply(epoch: Date, j2000Offset: Int): Date =
    new Date(epoch.getJ2000Day + j2000Offset)

  /**
   * Constructs a new `Date` instance from `Instant.now`
   *
   * This method creates a new `Date` object from the current instant of the time
   *
   * @return a `Date` instance representing the current Date
   */
  def now(): Date = new Date(Instant.now())
}