package org.abh80.nf
package core.time

import java.util.concurrent.atomic.AtomicReference


/**
 * A factory for obtaining singleton instances of various time scales.
 *
 * This object uses a thread-safe approach with `AtomicReference` to ensure that
 * only a single instance of each time scale is created and returned. This is
 * a form of the **Singleton design pattern**.
 */
object TimeScaleFactory {
  private val TTRef      = new AtomicReference[TTScale]()
  private val TDBRef     = new AtomicReference[TDBScale]()
  private val TAIRef     = new AtomicReference[TAIScale]()
  private val UTCRef     = new AtomicReference[UTCScale]()
  private val TDTRef     = new AtomicReference[TDTScale]()
  private val GLONASSRef = new AtomicReference[GLONASSScale]()
  private val IRNSSRef   = new AtomicReference[IRNSSScale]()
  private val GPSRef     = new AtomicReference[GPSScale]()

  /**
   * Returns a singleton instance of the Terrestrial Time (TT) scale.
   *
   * @return A `TimeScale` representing the Terrestrial Time.
   */
  def getTT: TimeScale = TTRef.updateAndGet(r => if r != null then r else TTScale())

  /**
   * Returns a singleton instance of the Barycentric Dynamical Time (TDB) scale.
   *
   * This scale is initialized based on the Terrestrial Time (TT) and the J2000.0 epoch.
   *
   * @return A `TimeScale` representing the Barycentric Dynamical Time.
   * @see [[TDBScale]]
   */
  def getTDB: TimeScale = TDBRef.updateAndGet(r => if r != null then r else TDBScale(getTT, EpochFactory.J2000_0))

  /**
   * Returns a singleton instance of the International Atomic Time (TAI) scale.
   *
   * @return A `TimeScale` representing the International Atomic Time.
   * @see [[TAIScale]]
   */
  def getTAI: TimeScale = TAIRef.updateAndGet(r => if r != null then r else TAIScale())

  /**
   * Returns a singleton instance of the Coordinated Universal Time (UTC) scale.
   *
   * This scale is initialized based on the International Atomic Time (TAI) scale.
   *
   * @return A `TimeScale` representing the Coordinated Universal Time.
   * @see [[UTCScale]]
   */
  def getUTC: TimeScale = UTCRef.updateAndGet(r => if r != null then r else UTCScale(getTAI))

  /**
   * Returns a singleton instance of the Terrestrial Dynamic Time (TDT) scale.
   *
   * This scale is initialized based on the International Atomic Time (TAI) scale.
   *
   * @return A `TimeScale` Terrestrial Dynamic Time.
   * @see [[TDTScale]]
   */
  def getTDT: TimeScale = TDTRef.updateAndGet(r => if r != null then r else TDTScale())

  /**
   * Returns a singleton instance of the GLONASS time scale.
   *
   * This scale is initialized based on the Coordinated Universal Time (UTC) scale.
   *
   * @return A `TimeScale` representing the GLONASS time.
   * @see [[GLONASSScale]]
   */
  def getGLONASS: TimeScale = GLONASSRef.updateAndGet(r => if r != null then r else GLONASSScale(getUTC))

  /**
   * Returns a singleton instance of the IRNSS time scale.
   *
   * @return A `TimeScale` representing the IRNSS time.
   * @see [[IRNSSScale]]
   */
  def getIRNSS: TimeScale = IRNSSRef.updateAndGet(r => if r != null then r else IRNSSScale())

  /**
   * Returns a singleton instance of the GPS time scale.
   *
   * @return A `TimeScale` representing the GPS time.
   * @see [[GPSScale]]
   */
  def getGPS: TimeScale = GPSRef.updateAndGet(r => if r != null then r else GPSScale())
}
