package org.abh80.nf.java

import org.abh80.nf.core.metrics.DistanceUnit

/**
 * Java-idiomatic facade for [[org.abh80.nf.core.metrics.DistanceUnit]].
 *
 * Static-style constructors for each unit plus named replacements for the `+`, `-`, `*`, `~`,
 * `ratio` operators.
 */
object Distances {

  /** Build a distance in meters. */
  def meters(value: Double): DistanceUnit.Meter = DistanceUnit.Meter(value)

  /** Build a distance in kilometers. */
  def kilometers(value: Double): DistanceUnit.Kilometer = DistanceUnit.Kilometer(value)

  /** Build a distance in astronomical units. */
  def astronomicalUnits(value: Double): DistanceUnit.AstronomicalUnit =
    DistanceUnit.AstronomicalUnit(value)

  /** Sum two distances, returning the result in the unit of `a`. */
  def add(a: DistanceUnit, b: DistanceUnit): DistanceUnit = a + b

  /** Subtract `b` from `a`, returning the result in the unit of `a`. */
  def subtract(a: DistanceUnit, b: DistanceUnit): DistanceUnit = a - b

  /** Scale a distance by a dimensionless factor. */
  def scale(a: DistanceUnit, factor: Double): DistanceUnit = a * factor

  /** Dimensionless ratio between two distances. */
  def ratio(a: DistanceUnit, b: DistanceUnit): Double = a.ratio(b)

  /** Approximate equality with a tolerance (meters). */
  def approxEquals(a: DistanceUnit, b: DistanceUnit, epsilon: Double): Boolean =
    a.~(b, epsilon)

  /** Approximate equality at the default 1e-10 tolerance. */
  def approxEquals(a: DistanceUnit, b: DistanceUnit): Boolean = a.~(b)
}
