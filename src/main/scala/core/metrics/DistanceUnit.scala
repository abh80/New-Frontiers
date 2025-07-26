package org.abh80.nf
package core.metrics

/** A trait representing different units of distance measurement.
 *
 * This trait provides a common interface for various distance units,
 * allowing conversion between different units and basic arithmetic operations.
 */
sealed trait DistanceUnit {
  // The numerical value in this unit (e.g., 1.0 for 1 km).
  def value: Double

  // Convert this distance to meters.
  def toMeter: Double

  // Create a new instance of this unit from a value in meters.
  def fromMeter(meters: Double): DistanceUnit

  // Symbol for display (e.g., "m", "km").
  def symbol: String

  override def toString: String = s"$value $symbol"
}

sealed trait DistanceUnitCompanion[T <: DistanceUnit] {
  def fromMeter(meters: Double): T
}

/** Companion object for DistanceUnit containing implementations of different distance units
 * and utility methods for distance calculations */
object DistanceUnit {
  private val AU_TO_METERS = 1.495978707e11

  /** Creates a specific distance unit from meters
   *
   * @param meters      the distance in meters
   * @param unitFactory the factory function to create the desired unit
   * @tparam T the type of distance unit to create
   * @return distance in the specified unit */
  def fromMeters[T <: DistanceUnit](meters: Double, unitFactory: Double => T): T = unitFactory(meters)

  /** Represents distance in meters
   *
   * @param value the distance value in meters */
  case class Meter(value: Double) extends DistanceUnit {
    override def toMeter: Double = value

    override def fromMeter(meters: Double): Meter = Meter(meters)

    override def symbol: String = "m"
  }

  /** Represents distance in kilometers
   *
   * @param value the distance value in kilometers */
  case class Kilometer(value: Double) extends DistanceUnit {
    override def toMeter: Double = value * 1000.0

    override def fromMeter(meters: Double): Kilometer = Kilometer.fromMeter(meters)

    override def symbol: String = "km"
  }

  /** Represents distance in astronomical units (AU)
   *
   * @param value the distance value in astronomical units */
  case class AstronomicalUnit(value: Double) extends DistanceUnit {
    override def toMeter: Double = value * AU_TO_METERS

    override def fromMeter(meters: Double): AstronomicalUnit = AstronomicalUnit.fromMeter(meters)

    override def symbol: String = "AU"
  }

  object Kilometer extends DistanceUnitCompanion[Kilometer] {
    def fromMeter(meters: Double): Kilometer = Kilometer(meters / 1000.0)

    def apply(value: Double): Kilometer = new Kilometer(value)
  }


  /** Provides binary operations for DistanceUnit
   *
   * @param self the DistanceUnit instance on which operations are performed */
  implicit class BinOp(self: DistanceUnit) {
    def +(second: DistanceUnit): DistanceUnit =
      self.fromMeter(self.toMeter + second.toMeter)

    def -(second: DistanceUnit): DistanceUnit =
      self.fromMeter(self.toMeter - second.toMeter)

    def ~(second: DistanceUnit, epsilon: Double = 1e-10): Boolean =
      Math.abs(self.toMeter - second.toMeter) < epsilon

    def *(second: DistanceUnit): DistanceUnit =
      self.fromMeter(self.toMeter * second.toMeter)

    def /(second: DistanceUnit): DistanceUnit =
      self.fromMeter(self.toMeter / second.toMeter)
  }

  object AstronomicalUnit extends DistanceUnitCompanion[AstronomicalUnit] {
    def fromMeter(meters: Double): AstronomicalUnit = AstronomicalUnit(meters / AU_TO_METERS)

    def apply(value: Double): AstronomicalUnit = new AstronomicalUnit(value)
  }

}