package org.abh80.nf
package util

/** Trait for date names (months, weekdays). */
trait DateName {
  /** Returns the name with first letter capitalized. */
  def toCapitalizedString: String = this.toString.toLowerCase.capitalize
}

/**
 * Base type for date formats represented as strings.
 *
 * Each implementation provides [[render]] which defines the complete string representation.
 * Use [[toString]] to retrieve the formatted date string.
 *
 * @param dd   Day
 * @param mm   Month
 * @param yyyy Year
 */
abstract class StringDateFormat(dd: Int, mm: Int, yyyy: Int) {
  protected val f_day: String   = DateUtil.formattedDay(dd)
  protected val f_month: String = DateUtil.formattedMonth(mm)
  protected val f_year: String  = DateUtil.formattedYear(yyyy)

  /** Returns the formatted date string for this format. */
  def render: String

  override def toString: String = render
}

/**
 * Enumeration for weekdays.
 * Use [[DateUtil.WeekUtil]] for conversions and checks.
 */
sealed abstract class Weekday(asInt: Int) extends DateName

object Weekday {
  case object MONDAY    extends Weekday(1)
  case object TUESDAY   extends Weekday(2)
  case object WEDNESDAY extends Weekday(3)
  case object THURSDAY  extends Weekday(4)
  case object FRIDAY    extends Weekday(5)
  case object SATURDAY  extends Weekday(6)
  case object SUNDAY    extends Weekday(7)

  val values: Array[Weekday] =
    Array(MONDAY, TUESDAY, WEDNESDAY, THURSDAY, FRIDAY, SATURDAY, SUNDAY)

  /** Looks up a weekday by its 0-based ordinal. */
  def fromOrdinal(i: Int): Weekday = values(i)
}

/**
 * Enumeration for months.
 * Use [[Month.getMonth]] to convert from integer.
 */
sealed abstract class Month(val asInt: Int) extends DateName {
  /** Returns the integer value of the month (1-12). */
  def getIntegerValue: Int = asInt
}

object Month {
  case object JANUARY   extends Month(1)
  case object FEBRUARY  extends Month(2)
  case object MARCH     extends Month(3)
  case object APRIL     extends Month(4)
  case object MAY       extends Month(5)
  case object JUNE      extends Month(6)
  case object JULY      extends Month(7)
  case object AUGUST    extends Month(8)
  case object SEPTEMBER extends Month(9)
  case object OCTOBER   extends Month(10)
  case object NOVEMBER  extends Month(11)
  case object DECEMBER  extends Month(12)

  val values: Array[Month] = Array(
    JANUARY, FEBRUARY, MARCH, APRIL, MAY, JUNE,
    JULY, AUGUST, SEPTEMBER, OCTOBER, NOVEMBER, DECEMBER
  )

  /** Looks up a month by its 0-based ordinal. */
  def fromOrdinal(i: Int): Month = values(i)

  /**
   * Converts integer (1-12) to Month enum.
   * @throws IllegalArgumentException if out of range.
   */
  @throws[IllegalArgumentException]
  def getMonth(asInt: Int): Month =
    if (asInt >= 1 && asInt <= 12) fromOrdinal(asInt - 1)
    else throw new IllegalArgumentException(s"integer value $asInt is not a qualified month")
}
