package org.abh80.nf
package frames

sealed abstract class DefaultFrames(name: String) {
  def getName: String = name
}

object DefaultFrames {
  case object GCRF extends DefaultFrames("GCRF")

  val values: Array[DefaultFrames] = Array(GCRF)
}
