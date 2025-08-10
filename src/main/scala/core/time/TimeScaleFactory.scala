package org.abh80.nf
package core.time

import java.util.concurrent.atomic.AtomicReference


object TimeScaleFactory {
  private val TTRef = AtomicReference[TTScale]()
  private val TDBRef = AtomicReference[TDBScale]()

  def getTT: TimeScale = {
    var ref = TTRef.get()
    
    if ref == null then {
      TTRef.set(TTScale())
      ref = TTRef.get()
    }
    
    ref
  }

  def getTDB: TimeScale = {
    var ref = TDBRef.get()
    
    if ref == null then {
      TDBRef.set(TDBScale(getTT, EpochFactory.J2000_0))
      ref = TDBRef.get()
    }
    
    ref
  }
}
