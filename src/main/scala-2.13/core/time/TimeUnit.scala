package org.abh80.nf
package core.time

/** Represents different units of time measurement in descending order of magnitude.
 *
 * Provides various time units from days down to attoseconds,
 * allowing for precise time measurements and conversions across different scales.
 */
sealed trait TimeUnit

object TimeUnit {
  /** Represents time in days (86,400 seconds) */
  case object DAYS         extends TimeUnit
  /** Represents time in hours (3,600 seconds) */
  case object HOURS        extends TimeUnit
  /** Represents time in minutes (60 seconds) */
  case object MINUTES      extends TimeUnit
  /** Represents time in seconds */
  case object SECONDS      extends TimeUnit
  /** Represents time in milliseconds (1/1,000 of a second) */
  case object MILLISECONDS extends TimeUnit
  /** Represents time in microseconds (1/1,000,000 of a second) */
  case object MICROSECONDS extends TimeUnit
  /** Represents time in nanoseconds (1/1,000,000,000 of a second) */
  case object NANOSECONDS  extends TimeUnit
  /** Represents time in picoseconds (1/1,000,000,000,000 of a second) */
  case object PICOSECONDS  extends TimeUnit
  /** Represents time in femtoseconds (1/1,000,000,000,000,000 of a second) */
  case object FEMTOSECONDS extends TimeUnit
  /** Represents time in attoseconds (1/1,000,000,000,000,000,000 of a second) */
  case object ATTOSECONDS  extends TimeUnit

  val values: Array[TimeUnit] = Array(
    DAYS, HOURS, MINUTES, SECONDS,
    MILLISECONDS, MICROSECONDS, NANOSECONDS,
    PICOSECONDS, FEMTOSECONDS, ATTOSECONDS
  )
}
