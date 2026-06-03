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
 *
 * The [[StringDateFormat]], [[DateName]], [[Weekday]], and [[Month]] types live as
 * top-level definitions in this package and are split between the Scala 2.13 and Scala 3
 * source directories.
 */
object DateUtil {
  /** Type alias for a date separator string. */
  type DateSeparator = String

  /** Formats year as a 4-digit string. */
  private[util] def formattedYear(year: Int) = String.format("%04d", year)

  /** Formats month as a 2-digit string. */
  private[util] def formattedMonth(month: Int) = String.format("%02d", month)

  /** Formats day as a 2-digit string. */
  private[util] def formattedDay(day: Int) = String.format("%02d", day)

  /**
   * ISO 8601 format (YYYY-MM-DD).
   * Use this for international standard.
   */
  final case class ISO8601(dd: Int, mm: Int, yyyy: Int) extends StringDateFormat(dd, mm, yyyy) {
    override def render: String = s"$f_year-$f_month-$f_day"
  }

  /**
   * RFC822 format (e.g., "Mon, 01 Jan 2024").
   * Use this for email headers and similar.
   */
  final case class RFC822(dd: Int, mm: Int, yyyy: Int, weekDay: Int) extends StringDateFormat(dd, mm, yyyy) {
    override def render: String =
      s"${WeekUtil.fromInt(weekDay).toCapitalizedString.slice(0, 3)}, $f_day ${Month.getMonth(mm).toCapitalizedString.slice(0, 3)} $yyyy"
  }

  /**
   * ANSI INCITS 30-1997 format (MM/DD/YYYY).
   * If you want American standard, use this.
   */
  final case class ANSI_INCITS_30_1997(dd: Int, mm: Int, yyyy: Int) extends StringDateFormat(dd, mm, yyyy) {
    override def render: String = s"$f_month/$f_day/$f_year"
  }

  /**
   * EN 28601 format (DD/MM/YYYY).
   * If you want European standard, use this.
   */
  final case class EN28601(dd: Int, mm: Int, yyyy: Int) extends StringDateFormat(dd, mm, yyyy) {
    override def render: String = s"$f_day/$f_month/$f_year"
  }

  /**
   * Asian format (YYYY.MM.DD).
   * If you want Asian style, use this.
   */
  final case class ASIAN(dd: Int, mm: Int, yyyy: Int) extends StringDateFormat(dd, mm, yyyy) {
    override def render: String = s"$f_year.$f_month.$f_day"
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
    @throws[IllegalArgumentException]
    def fromInt(asInt: Int): Weekday =
      if (asInt >= 1 && asInt <= 7) Weekday.fromOrdinal(asInt - 1)
      else throw new IllegalArgumentException(s"integer value $asInt is not a qualified weekday")
  }
}
