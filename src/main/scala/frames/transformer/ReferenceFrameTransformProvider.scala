package org.abh80.nf
package frames.transformer

import core.time.AbsoluteTime

trait ReferenceFrameTransformProvider {
  def getTransform(time: AbsoluteTime): ReferenceFrameTransform
}
