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
trait StringDateFormat(dd: Int, mm: Int, yyyy: Int) {
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
enum Weekday(asInt: Int) extends DateName {
  case MONDAY    extends Weekday(1)
  case TUESDAY   extends Weekday(2)
  case WEDNESDAY extends Weekday(3)
  case THURSDAY  extends Weekday(4)
  case FRIDAY    extends Weekday(5)
  case SATURDAY  extends Weekday(6)
  case SUNDAY    extends Weekday(7)
}

/**
 * Enumeration for months.
 * Use [[Month.getMonth]] to convert from integer.
 */
enum Month(val asInt: Int) extends DateName {
  case JANUARY   extends Month(1)
  case FEBRUARY  extends Month(2)
  case MARCH     extends Month(3)
  case APRIL     extends Month(4)
  case MAY       extends Month(5)
  case JUNE      extends Month(6)
  case JULY      extends Month(7)
  case AUGUST    extends Month(8)
  case SEPTEMBER extends Month(9)
  case OCTOBER   extends Month(10)
  case NOVEMBER  extends Month(11)
  case DECEMBER  extends Month(12)

  /** Returns the integer value of the month (1-12). */
  def getIntegerValue: Int = asInt
}

object Month {
  /**
   * Converts integer (1-12) to Month enum.
   * @throws IllegalArgumentException if out of range.
   */
  @throws[IllegalArgumentException]
  def getMonth(asInt: Int): Month =
    if (asInt >= 1 && asInt <= 12) Month.fromOrdinal(asInt - 1)
    else throw new IllegalArgumentException(s"integer value $asInt is not a qualified month")
}
