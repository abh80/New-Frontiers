package org.abh80.nf
package frames

import frames.transformer.ReferenceFrameTransformProvider

class Frame private(val parent: Option[Frame],
                    val depth: Int,
                    val name: String,
                    val pseudoInertial: Boolean,
                    val transformProvider: ReferenceFrameTransformProvider
                   ) {

  private def this(name: String, pseudoInertial: Boolean) = {
    this(None, 0, name, pseudoInertial, ???)
  }
}

case object Frame {
  private def getRoot = new Frame(DefaultFrames.GCRF.getName, true)
}