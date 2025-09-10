package org.abh80.nf
package matchers

import scala.math.abs
import org.scalatest.matchers.{BeMatcher, MatchResult}

class AlmostEqualsMatcher(expected: Double, tolerance: Double = 1e-6) extends BeMatcher[Double] {
  override def apply(actual: Double): MatchResult =
    val diff = abs(actual - expected)
    MatchResult(
      diff <= tolerance,
      s"$actual was not almost equal to $expected (difference was found $diff but tolerance was $tolerance)",
      s"$actual was almost equal to $expected (difference was found $diff and tolerance was $tolerance)"
    )
}

object AlmostEqualsMatcher {
  def almostEquals(expected: Double, tolerance: Double = 1e-6): AlmostEqualsMatcher =
    AlmostEqualsMatcher(expected, tolerance)

  implicit class AlmostEqualsIntegerOps(val actual: Double) extends AnyVal {
    def almostEquals(expected: Double, tolerance: Double = 1e-6): Boolean = {
      math.abs(actual - expected) <= tolerance
    }
  }
}