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
  private val TTRef = AtomicReference[TTScale]()
  private val TDBRef = AtomicReference[TDBScale]()
  private val TAIRef = AtomicReference[TAIScale]()
  private val UTCRef = AtomicReference[UTCScale]()
  private val TDTRef = AtomicReference[TDTScale]()
  private val GLONASSRef = AtomicReference[GLONASSScale]()
  private val IRNSSRef = AtomicReference[IRNSSScale]()

  /**
   * Returns a singleton instance of the Terrestrial Time (TT) scale.
   *
   * @return A `TimeScale` representing the Terrestrial Time.
   */
  def getTT: TimeScale = {
    var ref = TTRef.get()

    if ref == null then {
      TTRef.set(TTScale())
      ref = TTRef.get()
    }

    ref
  }

  /**
   * Returns a singleton instance of the Barycentric Dynamical Time (TDB) scale.
   *
   * This scale is initialized based on the Terrestrial Time (TT) and the J2000.0 epoch.
   *
   * @return A `TimeScale` representing the Barycentric Dynamical Time.
   * @see [[TDBScale]]
   */
  def getTDB: TimeScale = {
    var ref = TDBRef.get()

    if ref == null then {
      TDBRef.set(TDBScale(getTT, EpochFactory.J2000_0))
      ref = TDBRef.get()
    }

    ref
  }

  /**
   * Returns a singleton instance of the International Atomic Time (TAI) scale.
   *
   * @return A `TimeScale` representing the International Atomic Time.
   * @see [[TAIScale]]
   */
  def getTAI: TimeScale = {
    var ref = TAIRef.get()

    if ref == null then {
      TAIRef.set(TAIScale())
      ref = TAIRef.get()
    }

    ref
  }

  /**
   * Returns a singleton instance of the Coordinated Universal Time (UTC) scale.
   *
   * This scale is initialized based on the International Atomic Time (TAI) scale.
   *
   * @return A `TimeScale` representing the Coordinated Universal Time.
   * @see [[TTScale]]
   */
  def getUTC: TimeScale = {
    var ref = UTCRef.get()

    if ref == null then {
      UTCRef.set(UTCScale(getTAI))
      ref = UTCRef.get()
    }

    ref
  }

  /**
   * Returns a singleton instance of the Terrestrial Dynamic Time (TDT) scale.
   *
   * This scale is initialized based on the International Atomic Time (TAI) scale.
   *
   * @return A `TimeScale` Terrestrial Dynamic Time.
   * @see [[TDTScale]]
   */
  def getTDT: TimeScale = {
    var ref = TDTRef.get()

    if ref == null then {
      TDTRef.set(TDTScale())
      ref = TDTRef.get()
    }

    ref
  }

  /**
   * Returns a singleton instance of the GLONASS time scale.
   *
   * This scale is initialized based on the Coordinated Universal Time (UTC) scale.
   *
   * @return A `TimeScale` representing the GLONASS time.
   * @see [[GLONASSScale]]
   */
  def getGLONASS: TimeScale = {
    var ref = GLONASSRef.get()

    if ref == null then {
      GLONASSRef.set(GLONASSScale(getUTC))
      ref = GLONASSRef.get()
    }

    ref
  }

  /**
   * Returns a singleton instance of the IRNSS time scale.
   *
   * This scale is initialized based on the Coordinated Universal Time (UTC) scale.
   *
   * @return A `TimeScale` representing the IRNSS time.
   * @see [[IRNSSScale]]
   */
  def getIRNSS: TimeScale = {
    var ref = IRNSSRef.get()

    if ref == null then {
      IRNSSRef.set(IRNSSScale())
      ref = IRNSSRef.get()
    }

    ref
  }
}
