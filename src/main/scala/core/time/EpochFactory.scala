package org.abh80.nf
package core.time

/** Factory object providing commonly used epoch reference dates in astronomy and time systems.
 *
 * This object contains predefined epoch dates that are frequently used in astronomical
 * calculations and various time reference systems. These epochs serve as reference
 * points for different time scales and coordinate systems.
 *
 * @see [[https://spsweb.fltops.jpl.nasa.gov/portaldataops/mpg/MPG_Docs/MPG%20Book/Release/Chapter2-TimeScales.pdf]]
 */
object EpochFactory {
  /** The J2000.0 epoch, which is the fundamental epoch for the ICRF reference frame.
   * Defined as January 1, 2000, at 12:00 TT (Terrestrial Time).
   */
  val J2000_0: AbsoluteTime = AbsoluteTime(Date.J2000_0, Time.AFTERNOON, TimeScaleFactory.getTT)

  /** The J1900 epoch, Defined as January 1, 1900, at 12:00 TT. A century before [[EpochFactory.J2000_0]].
   * This is useful to implement leap seconds for [[UTCScale]], since leap seconds started the year ago 2000.
   *
   * @see [[https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/FORTRAN/spicelib/j1900.html]] */
  val J1900: AbsoluteTime = AbsoluteTime(Date(1900, 1, 1), Time.AFTERNOON, TimeScaleFactory.getTT)

  /** The Julian epoch, which marks the beginning of the Julian calendar.
   * Defined as January 1, 4713, BCE at 12:00 UT.
   */
  val JULIAN: AbsoluteTime = AbsoluteTime(Date.JULIAN, Time.AFTERNOON, TimeScaleFactory.getTT)
  /** The Unix epoch, used as the starting point for Unix timestamps.
   * Defined as January 1, 1970, at 00:00:00 UTC.
   */
  val UNIX: AbsoluteTime = AbsoluteTime(Date.UNIX, Time.MIDNIGHT, TimeScaleFactory.getUTC)
  /** The CXCSEC epoch, used by the Chandra X-ray Observatory.
   * Defined as January 1, 1998, at 00:00:00 TT.
   */
  val CXCSEC: AbsoluteTime = AbsoluteTime(Date.CXCSEC, Time.MIDNIGHT, TimeScaleFactory.getTT)
  /** The GPS epoch, used by the Global Positioning System.
   * Defined as January 6, 1980, at 00:00:00 UTC.
   */
  val GPS: AbsoluteTime = AbsoluteTime(Date.GPS, Time.MIDNIGHT, TimeScaleFactory.getUTC)

  /** The GLONASS epoch, used by the Russian Global Navigation Satellite System.
   * Defined as January 1, 1996, at 00:00:00+03:00 UTC. However, this timescale follows an atomic scale after the UTC, this is why we add a 29-second
   * offset for 1996 and then count as so in atomic timescale.
   */
  val GLONASS: AbsoluteTime = AbsoluteTime(Date.GLONASS, Time(0, 0, 29.0, 180), TimeScaleFactory.getTAI)

  /** The NA */
  val NAVIC: AbsoluteTime = AbsoluteTime(Date.NAVIC, Time.MIDNIGHT, TimeScaleFactory.getIRNSS)
}
