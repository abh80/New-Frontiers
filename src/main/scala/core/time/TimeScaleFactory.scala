package org.abh80.nf
package core.time

/**
 * A factory for obtaining singleton instances of various time scales.
 *
 * This object uses Scala `lazy val` initialization to ensure that each time
 * scale is created at most once in a thread-safe way.
 */
object TimeScaleFactory {
  private lazy val ttScale: TTScale = TTScale()
  private lazy val tdbScale: TDBScale = TDBScale(getTT, EpochFactory.J2000_0)
  private lazy val taiScale: TAIScale = TAIScale()
  private lazy val utcScale: UTCScale = UTCScale(getTAI)
  private lazy val tdtScale: TDTScale = TDTScale()
  private lazy val glonassScale: GLONASSScale = GLONASSScale(getUTC)
  private lazy val irnssScale: IRNSSScale = IRNSSScale()
  private lazy val gpsScale: GPSScale = GPSScale()

  /**
   * Returns a singleton instance of the Terrestrial Time (TT) scale.
   *
   * @return A `TimeScale` representing the Terrestrial Time.
   */
  def getTT: TimeScale = ttScale

  /**
   * Returns a singleton instance of the Barycentric Dynamical Time (TDB) scale.
   *
   * This scale is initialized based on the Terrestrial Time (TT) and the J2000.0 epoch.
   *
   * @return A `TimeScale` representing the Barycentric Dynamical Time.
   * @see [[TDBScale]]
   */
  def getTDB: TimeScale = tdbScale

  /**
   * Returns a singleton instance of the International Atomic Time (TAI) scale.
   *
   * @return A `TimeScale` representing the International Atomic Time.
   * @see [[TAIScale]]
   */
  def getTAI: TimeScale = taiScale

  /**
   * Returns a singleton instance of the Coordinated Universal Time (UTC) scale.
   *
   * This scale is initialized based on the International Atomic Time (TAI) scale.
   *
   * @return A `TimeScale` representing the Coordinated Universal Time.
   * @see [[UTCScale]]
   */
  def getUTC: TimeScale = utcScale

  /**
   * Returns a singleton instance of the Terrestrial Dynamic Time (TDT) scale.
   *
   * This scale is initialized based on the International Atomic Time (TAI) scale.
   *
   * @return A `TimeScale` Terrestrial Dynamic Time.
   * @see [[TDTScale]]
   */
  def getTDT: TimeScale = tdtScale

  /**
   * Returns a singleton instance of the GLONASS time scale.
   *
   * This scale is initialized based on the Coordinated Universal Time (UTC) scale.
   *
   * @return A `TimeScale` representing the GLONASS time.
   * @see [[GLONASSScale]]
   */
  def getGLONASS: TimeScale = glonassScale

  /**
   * Returns a singleton instance of the IRNSS time scale.
   *
   * @return A `TimeScale` representing the IRNSS time.
   * @see [[IRNSSScale]]
   */
  def getIRNSS: TimeScale = irnssScale

  /**
   * Returns a singleton instance of the GPS time scale.
   *
   * @return A `TimeScale` representing the GPS time.
   * @see [[GPSScale]]
   */
  def getGPS: TimeScale = gpsScale
}
