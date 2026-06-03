package org.abh80.nf.java

import org.abh80.nf.core.metrics.AngleUnit

/**
 * Java-idiomatic facade for [[org.abh80.nf.core.metrics.AngleUnit]].
 *
 * Provides static-style factories and renames the `+`, `-`, `*`, `~`, `ratio` operators.
 */
object Angles {

  /** Build an angle in radians. */
  def radians(value: Double): AngleUnit.Radian = AngleUnit.Radian(value)

  /** Build an angle in degrees. */
  def degrees(value: Double): AngleUnit.Degree = AngleUnit.Degree(value)

  /** Build an angle in arcminutes. */
  def arcMinutes(value: Double): AngleUnit.ArcMinute = AngleUnit.ArcMinute(value)

  /** Build an angle in arcseconds. */
  def arcSeconds(value: Double): AngleUnit.ArcSecond = AngleUnit.ArcSecond(value)

  /** Build an angle in hour angles. */
  def hourAngle(value: Double): AngleUnit.HourAngle = AngleUnit.HourAngle(value)

  /** Sum two angles, returning the result in the unit of `a`. */
  def add(a: AngleUnit, b: AngleUnit): AngleUnit = a + b

  /** Subtract `b` from `a`, returning the result in the unit of `a`. */
  def subtract(a: AngleUnit, b: AngleUnit): AngleUnit = a - b

  /** Scale an angle by a dimensionless factor. */
  def scale(a: AngleUnit, factor: Double): AngleUnit = a * factor

  /** Dimensionless ratio between two angles. */
  def ratio(a: AngleUnit, b: AngleUnit): Double = a.ratio(b)

  /** Approximate equality with a tolerance (radians). */
  def approxEquals(a: AngleUnit, b: AngleUnit, epsilon: Double): Boolean =
    a.~(b, epsilon)

  /** Approximate equality at the default 1e-10 tolerance. */
  def approxEquals(a: AngleUnit, b: AngleUnit): Boolean = a.~(b)
}
