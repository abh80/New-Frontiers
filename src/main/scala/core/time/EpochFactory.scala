package org.abh80.nf
package core.time

/** Factory object providing commonly used epoch reference dates in astronomy and time systems.
 *
 * This object contains predefined epoch dates that are frequently used in astronomical
 * calculations and various time reference systems. These epochs serve as reference
 * points for different time scales and coordinate systems.
 *
 * Reference: https://spsweb.fltops.jpl.nasa.gov/portaldataops/mpg/MPG_Docs/MPG%20Book/Release/Chapter2-TimeScales.pdf
 */
object EpochFactory {
  /** The J2000.0 epoch, which is the fundamental epoch for the ICRF reference frame.
   * Defined as January 1, 2000, at 12:00 TT (Terrestrial Time).
   */
  val J2000_0: AbsoluteTime = AbsoluteTime(Date(2000, 1, 1), Time(12,  0, TimeFormat.Zero), TimeScaleFactory.getTT)
  
  /** The Julian epoch, which marks the beginning of the Julian calendar.
   * Defined as January 1, 4713, BCE at 12:00 UT.
   */
  val JULIAN: Date = Date(-4713, 1, 1)
  /** The Unix epoch, used as the starting point for Unix timestamps.
   * Defined as January 1, 1970, at 00:00:00 UTC.
   */
  val UNIX: Date = Date(1970, 1, 1)
  /** The CXCSEC epoch, used by the Chandra X-ray Observatory.
   * Defined as January 1, 1998, at 00:00:00 TT.
   */
  val CXCSEC: Date = Date(1998, 1, 1)
  /** The GPS epoch, used by the Global Positioning System.
   * Defined as January 6, 1980, at 00:00:00 UTC.
   */
  val GPS: Date = Date(1980, 1, 6)
}
