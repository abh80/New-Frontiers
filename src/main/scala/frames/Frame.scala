package org.abh80.nf
package frames

import core.time.AbsoluteTime
import frames.transformer.{ReferenceFrameTransform, ReferenceFrameTransformProvider}

class Frame private(val parent: Option[Frame],
                    val depth: Int,
                    val name: String,
                    val pseudoInertial: Boolean,
                    val transformProvider: ReferenceFrameTransformProvider
                   ) {

  // Stub provider used until a real root transform is implemented.
  private def this(name: String, pseudoInertial: Boolean) =
    this(None, 0, name, pseudoInertial, Frame.StubProvider)
}

case object Frame {
  /** Placeholder until the frames module is implemented. */
  private object StubProvider extends ReferenceFrameTransformProvider {
    override def getTransform(time: AbsoluteTime): ReferenceFrameTransform =
      throw new UnsupportedOperationException("Root frame transform provider not yet implemented")
  }

  private def getRoot = new Frame(DefaultFrames.GCRF.getName, true)
}
