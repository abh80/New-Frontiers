package org.abh80.nf
package core.metrics

sealed trait AngleUnit {
  // The numerical value in this unit (eg, 10 degrees or 2.5 radians).
  def value: Double

  // Convert this distance to meters.
  def toRadian: Double

  // Create a new instance of this unit from a value in rads.
  def fromRadians(rad: Double): AngleUnit

  // Symbol for display (e.g., "rad", "degree").
  def symbol: String

  override def toString: String = s"$value $symbol"
}

object AngleUnit {
  case class Radian(value: Double) extends AngleUnit {
    override def toRadian: Double = value

    override def fromRadians(rad: Double): Radian = Radian(rad)

    override def symbol: String = "rad"
  }

  case class Degree(value: Double) extends AngleUnit {
    override def toRadian: Double = value * Math.PI / 180.0

    override def fromRadians(rad: Double): Degree = Degree(rad * 180.0 / Math.PI)

    override def symbol: String = "°"
  }

  case class ArcMinute(value: Double) extends AngleUnit {
    override def toRadian: Double = value * Math.PI / (180.0 * 60.0)

    override def fromRadians(rad: Double): ArcMinute = ArcMinute(rad * (180.0 * 60.0) / Math.PI)

    override def symbol: String = "′"
  }

  case class ArcSecond(value: Double) extends AngleUnit {
    override def toRadian: Double = value * Math.PI / (180.0 * 3600.0)

    override def fromRadians(rad: Double): ArcSecond = ArcSecond(rad * (180.0 * 3600.0) / Math.PI)

    override def symbol: String = "″"
  }

  case class HourAngle(value: Double) extends AngleUnit {
    override def toRadian: Double = value * Math.PI / 12.0

    override def fromRadians(rad: Double): HourAngle = HourAngle(rad * 12.0 / Math.PI)

    override def symbol: String = "h"
  }

  def fromRadians[T <: AngleUnit](rad: Double, unitFactory: Double => T): T = unitFactory(rad)
}