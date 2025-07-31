package org.abh80.nf
package core.time

/** Represents different units of time measurement in descending order of magnitude.
 * 
 * This enum provides various time units from days down to attoseconds,
 * allowing for precise time measurements and conversions across different scales.
 */
enum TimeUnit {
  /** Represents time in days (86,400 seconds) */
  case DAYS
  /** Represents time in hours (3,600 seconds) */
  case HOURS
  /** Represents time in minutes (60 seconds) */
  case MINUTES
  /** Represents time in seconds */
  case SECONDS
  /** Represents time in milliseconds (1/1,000 of a second) */
  case MILLISECONDS
  /** Represents time in microseconds (1/1,000,000 of a second) */
  case MICROSECONDS
  /** Represents time in nanoseconds (1/1,000,000,000 of a second) */
  case NANOSECONDS
  /** Represents time in picoseconds (1/1,000,000,000,000 of a second) */
  case PICOSECONDS
  /** Represents time in femtoseconds (1/1,000,000,000,000,000 of a second) */
  case FEMTOSECONDS
  /** Represents time in attoseconds (1/1,000,000,000,000,000,000 of a second) */
  case ATTOSECONDS
}