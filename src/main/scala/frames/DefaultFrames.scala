package org.abh80.nf
package frames

enum DefaultFrames(name: String) {
  case GCRF extends DefaultFrames("GCRF")
  
  def getName: String = name
}