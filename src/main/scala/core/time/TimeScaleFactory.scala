package org.abh80.nf
package core.time

import java.util.concurrent.atomic.AtomicReference


object TimeScaleFactory {
  private val TTRef = AtomicReference[TTScale]()

  def getTT: TimeScale = {
    var ref = TTRef.get()
    
    if ref == null then {
      TTRef.set(TTScale())
      ref = TTRef.get()
    }
    
    ref
  }
}
