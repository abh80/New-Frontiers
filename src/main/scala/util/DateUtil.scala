package org.abh80.nf
package util

/**
 * Utility object for date formatting and manipulation.
 * 
 * Provides various date formats. 
 * If you want American standard (MM/DD/YYYY), use ANSI_INCITS_30_1997.
 * If you want ISO standard (YYYY-MM-DD), use ISO8601.
 * If you want Asian style (YYYY.MM.DD), use ASIAN.
 * If you want European style (DD/MM/YYYY), use EN28601.
 * RFC822 is for email headers and similar.
 */
object DateUtil {
  /** Type alias for a date separator string. */
  type DateSeparator = String

  /** Formats year as a 4-digit string. */
  private def formattedYear(year: Int) = String.format("%04d", year)

  /** Formats month as a 2-digit string. */
  private def formattedMonth(month: Int) = String.format("%02d", month)

  /** Formats day as a 2-digit string. */
  private def formattedDay(day: Int) = String.format("%02d", day)

  /**
   * Base trait for date formats represented as strings.
   * 
   * @param dd Day
   * @param mm Month
   * @param yyyy Year
   */
  sealed trait StringDateFormat(dd: Int, mm: Int, yyyy: Int) {
    /** Tuple of formatted date parts, order depends on format. */
    val formatted3partitionTuple: (String, String, String)
    /** Separator used in string representation. */
    val sep: DateSeparator = DateSeparator.hyphen
    protected val f_day: String = formattedDay(dd)
    protected val f_month: String = formattedMonth(mm)
    protected val f_year: String = formattedYear(yyyy)

    /** Returns the formatted date string. */
    override def toString: String = Array(formatted3partitionTuple._1, formatted3partitionTuple._2, formatted3partitionTuple._3).mkString(sep)
  }

  /**
   * Trait for date names (months, weekdays).
   */
  sealed trait DateName {
    /** Returns the name with first letter capitalized. */
    def toCapitalizedString: String = this.toString.toLowerCase.capitalize
  }

  /**
   * ISO 8601 format (YYYY-MM-DD).
   * Use this for international standard.
   */
  final case class ISO8601(dd: Int, mm: Int, yyyy: Int) extends StringDateFormat(dd, mm, yyyy) {
    override val formatted3partitionTuple: (String, String, String) = (f_year, f_month, f_day)
  }

  /**
   * RFC822 format (e.g., "Mon, 01 Jan 2024").
   * Use this for email headers and similar.
   */
  final case class RFC822(dd: Int, mm: Int, yyyy: Int, weekDay: Int) extends StringDateFormat(dd, mm, yyyy) {
    override val formatted3partitionTuple: (String, String, String) = null

    override def toString: String = s"${WeekUtil.fromInt(weekDay).toCapitalizedString.slice(0, 3)}, $f_day ${Month.getMonth(mm).toCapitalizedString.slice(0, 3)} $yyyy"
  }

  /**
   * ANSI INCITS 30-1997 format (MM/DD/YYYY).
   * If you want American standard, use this.
   */
  final case class ANSI_INCITS_30_1997(dd: Int, mm: Int, yyyy: Int) extends StringDateFormat(dd, mm, yyyy) {
    override val sep: DateSeparator = DateSeparator.slash

    override val formatted3partitionTuple: (String, String, String) = (f_month, f_day, f_year)
  }

  /**
   * EN 28601 format (DD/MM/YYYY).
   * If you want European standard, use this.
   */
  final case class EN28601(dd: Int, mm: Int, yyyy: Int) extends StringDateFormat(dd, mm, yyyy) {
    override val formatted3partitionTuple: (String, String, String) = (f_day, f_month, f_year)

    override val sep: DateSeparator = DateSeparator.slash
  }

  /**
   * Asian format (YYYY.MM.DD).
   * If you want Asian style, use this.
   */
  final case class ASIAN(dd: Int, mm: Int, yyyy: Int) extends StringDateFormat(dd, mm, yyyy) {
    override val sep: DateSeparator = DateSeparator.dot

    override val formatted3partitionTuple: (String, String, String) = (f_year, f_month, f_day)
  }

  /**
   * Enumeration for weekdays.
   * Use WeekUtil for conversions and checks.
   */
  enum Weekday(asInt: Int) extends DateName {
    case MONDAY extends Weekday(1)
    case TUESDAY extends Weekday(2)
    case WEDNESDAY extends Weekday(3)
    case THURSDAY extends Weekday(4)
    case FRIDAY extends Weekday(5)
    case SATURDAY extends Weekday(6)
    case SUNDAY extends Weekday(7)
  }

  /**
   * Enumeration for months.
   * Use Month.getMonth to convert from integer.
   */
  enum Month(val asInt: Int) extends DateName {
    case JANUARY extends Month(1)
    case FEBRUARY extends Month(2)
    case MARCH extends Month(3)
    case APRIL extends Month(4)
    case MAY extends Month(5)
    case JUNE extends Month(6)
    case JULY extends Month(7)
    case AUGUST extends Month(8)
    case SEPTEMBER extends Month(9)
    case OCTOBER extends Month(10)
    case NOVEMBER extends Month(11)
    case DECEMBER extends Month(12)

    /** Returns the integer value of the month (1-12). */
    def getIntegerValue: Int = asInt
  }

  /**
   * Common date separators.
   */
  object DateSeparator {
    /** Hyphen separator ("-"), used in ISO. */
    val hyphen: DateSeparator = "-"
    /** Slash separator ("/"), used in American and European formats. */
    val slash: DateSeparator = "/"
    /** Dot separator ("."), used in Asian format. */
    val dot: DateSeparator = "."
  }

  /**
   * Utilities for working with weekdays.
   */
  case object WeekUtil {
    /** Returns true if the day is a weekday (Mon-Fri). */
    def isWeekday(day: Weekday): Boolean = !isWeekend(day)

    /** Returns true if the day is a weekend (Sat-Sun). */
    def isWeekend(day: Weekday): Boolean = day match {
      case Weekday.SATURDAY | Weekday.SUNDAY => true
      case _ => false
    }

    /**
     * Converts integer (1-7) to Weekday enum.
     * @throws IllegalArgumentException if out of range.
     */
    def fromInt(asInt: Int): Weekday =
      Weekday.values.find(_.ordinal + 1 == asInt).getOrElse(throw new IllegalArgumentException(s"integer value $asInt is not a qualified weekday"))
  }

  /**
   * Utilities for working with months.
   */
  object Month {
    /**
     * Converts integer (1-12) to Month enum.
     * @throws IllegalArgumentException if out of range.
     */
    @throws[IllegalArgumentException]
    def getMonth(asInt: Int): Month =
      Month.values.find(_.ordinal + 1 == asInt).getOrElse(throw new IllegalArgumentException(s"integer value $asInt is not a qualified month"))
  }
}
