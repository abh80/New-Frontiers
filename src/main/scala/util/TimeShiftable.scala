package org.abh80.nf
package util

import core.time.TimeFormat

/**
 * This trait is implemented by the Class that needs to represent an object that could be shifted by a certain time.
 * ==Math Note==
 * Shifting is equivalent to:
 *   {{{
 *   new_time = old_time + Δt
 *   }}}
 *
 * @tparam T Type of the object
 *
 */
trait TimeShiftable[T] {
  /** Get a timeshifted instance of [[T]]
   *
   * @param dt time to be shifted in seconds
   */
  def ++(dt: Double): T

  /** Get a timeshifted instance of [[T]]
   *
   * @param timeFormat time to be shifted in TimeFormat
   * @see [[TimeFormat]]
   */
  def ++(timeFormat: TimeFormat): T = ++(timeFormat.toDouble)

  /** Get a timeshifted instance of [[T]]
   *
   * @param dt time to be shifted in seconds
   */
  def shiftBy(dt: Double): T = ++(dt)

  /** Get a timeshifted instance of [[T]]
   *
   * @param timeFormat time to be shifted in TimeFormat
   * @see [[TimeFormat]]
   */
  def shiftBy(timeFormat: TimeFormat): T = ++(timeFormat.toDouble)
}
