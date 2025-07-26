package org.abh80.nf
package core.metrics

/**
 * Represents an angle unit with a value and provides conversion to radians.
 */
sealed trait AngleUnit {
  /**
   * The numerical value in this unit (e.g., 10 degrees or 2.5 radians).
   */
  def value: Double

  /**
   * Converts this angle to radians.
   * @return the angle in radians
   */
  def toRadians: Double

  /**
   * Creates a new instance of this unit from a value in radians.
   * @param rad the angle in radians
   * @return a new instance of this angle unit
   */
  def fromRadians(rad: Double): AngleUnit

  /**
   * The symbol for display (e.g., "rad", "degree").
   */
  def symbol: String

  override def toString: String = s"$value $symbol"
}

/**
 * Companion trait for angle units, providing a method to create an instance from radians.
 * @tparam T the type of AngleUnit
 */
sealed trait AngleUnitCompanion[T <: AngleUnit] {
  /**
   * Creates an instance of the angle unit from radians.
   * @param radians the angle in radians
   * @return an instance of the angle unit
   */
  def fromRadians(radians: Double): T
}

/**
 * Provides factory methods and case classes for different angle units.
 */
object AngleUnit {
  /**
   * Creates an angle unit of type T from radians using the provided factory.
   * @param rad the angle in radians
   * @param unitFactory a function to create an instance of T from a Double
   * @tparam T the type of AngleUnit
   * @return an instance of T
   */
  def fromRadians[T <: AngleUnit](rad: Double, unitFactory: Double => T): T = unitFactory(rad)

  /**
   * Represents an angle in radians.
   * @param value the angle value in radians
   */
  case class Radian(value: Double) extends AngleUnit {
    override def toRadians: Double = value

    override def fromRadians(rad: Double): Radian = Radian(rad)

    override def symbol: String = "rad"
  }

  /**
   * Represents an angle in degrees.
   * @param value the angle value in degrees
   */
  case class Degree(value: Double) extends AngleUnit {
    override def toRadians: Double = value * Math.PI / 180.0

    override def fromRadians(rad: Double): Degree = Degree.fromRadians(rad)

    override def symbol: String = "°"
  }

  /**
   * Represents an angle in arcminutes.
   * @param value the angle value in arcminutes
   */
  case class ArcMinute(value: Double) extends AngleUnit {
    override def toRadians: Double = value * Math.PI / (180.0 * 60.0)

    override def fromRadians(rad: Double): ArcMinute = ArcMinute.fromRadians(rad)

    override def symbol: String = "′"
  }

  /**
   * Represents an angle in arcseconds.
   * @param value the angle value in arcseconds
   */
  case class ArcSecond(value: Double) extends AngleUnit {
    override def toRadians: Double = value * Math.PI / (180.0 * 3600.0)

    override def fromRadians(rad: Double): ArcSecond = ArcSecond.fromRadians(rad)

    override def symbol: String = "″"
  }

  /**
   * Represents an angle in hour angles.
   * @param value the angle value in hour angles
   */
  case class HourAngle(value: Double) extends AngleUnit {
    override def toRadians: Double = value * Math.PI / 12.0

    override def fromRadians(rad: Double): HourAngle = HourAngle.fromRadians(rad)

    override def symbol: String = "h"
  }

  /**
   * Companion object for Degree, provides conversion from radians.
   */
  case object Degree extends AngleUnitCompanion[Degree] {
    override def fromRadians(rad: Double): Degree = Degree(rad * 180.0 / Math.PI)
  }

  /**
   * Companion object for ArcMinute, provides conversion from radians.
   */
  case object ArcMinute extends AngleUnitCompanion[ArcMinute] {
    override def fromRadians(rad: Double): ArcMinute = ArcMinute(rad * (180.0 * 60.0) / Math.PI)
  }

  /**
   * Companion object for ArcSecond, provides conversion from radians.
   */
  case object ArcSecond extends AngleUnitCompanion[ArcSecond] {
    override def fromRadians(rad: Double): ArcSecond = ArcSecond(rad * (180.0 * 3600.0) / Math.PI)
  }

  /**
   * Companion object for HourAngle, provides conversion from radians.
   */
  case object HourAngle extends AngleUnitCompanion[HourAngle] {
    override def fromRadians(rad: Double): HourAngle = HourAngle(rad * 12.0 / Math.PI)
  }

  /**
   * Provides arithmetic and comparison operations for AngleUnit instances.
   * @param self the AngleUnit instance
   */
  implicit class BinOp(self: AngleUnit) {
    /**
     * Adds two angle units.
     * @param second the other angle unit
     * @return the sum as an AngleUnit
     */
    def +(second: AngleUnit): AngleUnit =
      self.fromRadians(self.toRadians + second.toRadians)

    /**
     * Subtracts one angle unit from another.
     * @param second the other angle unit
     * @return the difference as an AngleUnit
     */
    def -(second: AngleUnit): AngleUnit =
      self.fromRadians(self.toRadians - second.toRadians)

    /**
     * Compares two angle units for approximate equality.
     * @param second the other angle unit
     * @param epsilon the tolerance for comparison
     * @return true if the difference is less than epsilon, false otherwise
     */
    def ~(second: AngleUnit, epsilon: Double = 1e-10): Boolean =
      Math.abs(self.toRadians - second.toRadians) < epsilon

    /**
     * Multiplies two angle units.
     * @param second the other angle unit
     * @return the product as an AngleUnit
     */
    def *(second: AngleUnit): AngleUnit =
      self.fromRadians(self.toRadians * second.toRadians)

    /**
     * Divides one angle unit by another.
     * @param second the other angle unit
     * @return the quotient as an AngleUnit
     */
    def /(second: AngleUnit): AngleUnit =
      self.fromRadians(self.toRadians / second.toRadians)
  }
}