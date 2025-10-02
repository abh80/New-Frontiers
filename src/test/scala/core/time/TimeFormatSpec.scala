package org.abh80.nf
package core.time

import scala.math.abs
import org.scalatest.funsuite.AnyFunSuite
import matchers.AlmostEqualsMatcher._
import scala.util.Random
import org.scalatest.matchers.should.Matchers

class TimeFormatSpec extends AnyFunSuite with Matchers {
  test("time format addition") {
    checkResult(TimeFormat(24L, 940L) + TimeFormat(36L, 7400L), 60L, 8340L)
    checkResult(TimeFormat(24L, 940L) + TimeFormat(-20L, -1000L), 3L, 999999999999999940L)
  }

  test("time format subtraction") {
    checkResult(TimeFormat(1234L, 7028L) - TimeFormat(781L, 1043L), 453L, 5985L)
    checkResult(TimeFormat(1L, 0L) - TimeFormat(2L, 1L), -2L, 999999999999999999L)
  }

  test("toString value") {
    assertResult("0.000000000000000000")(TimeFormat.Zero.toString)
    assertResult("0.000001000000000000")(TimeFormat.MICROSECOND.toString)
    assertResult("0.001000000000000000")(TimeFormat.MILLISECOND.toString)
    assertResult("1.000000000000000000")(TimeFormat.SECOND.toString)
    assertResult("60.000000000000000000")(TimeFormat.MINUTE.toString)
    assertResult("3600.000000000000000000")(TimeFormat.HOUR.toString)
    assertResult("86400.000000000000000000")(TimeFormat.DAY.toString)
  }

  test("time format constants") {
    checkResult(TimeFormat.MICROSECOND, 0L, 1_000_000_000_000L)
    checkResult(TimeFormat.MILLISECOND, 0L, 1_000_000_000_000_000L)
    checkResult(TimeFormat.SECOND, 1L, 0L)
    checkResult(TimeFormat.MINUTE, 60L, 0L)
    checkResult(TimeFormat.HOUR, 3600L, 0L)
    checkResult(TimeFormat.DAY, 86400L, 0L)
  }

  test("time format multiplication") {
    assertThrows[IllegalArgumentException](TimeFormat(1L, 1L) * -1) // scalar cannot be negative

    checkResult(TimeFormat(1L, 7L) * 0L, 0L, 0L)
    checkResult(TimeFormat(1L, 7L) * 1L, 1L, 7L)
    checkResult(TimeFormat(1L, 75L) * 5L, 5L, 375L)
    checkResult(TimeFormat(1234L, 123456789012345678L).*(7233L), 8926414L, 962954926296288974L)
    checkResult(TimeFormat(1234L, 999999999999999999L).*(23012696L), 28420679559L, 999999999976987304L)
    checkResult(TimeFormat(1234L, 999999999999999999L).*(123456789012L), 152469134429819L, 999999876543210988L)

    assertThrows[ArithmeticException](TimeFormat(10000000000L, 1L) * 123456789012L) //overflow error
    assertThrows[ArithmeticException](TimeFormat(922382683L, 717054400620018329L) * 1573105907129L) // overflow error

  }

  test("time format division") {
    assertThrows[IllegalArgumentException](TimeFormat(1L, 1L) / 0L)

    checkResult(TimeFormat(1234L, 123456789012345678L) / 1L, 1234L, 123456789012345678L)
    checkResult(TimeFormat(1234L, 999999999999999999L) / 7L, 176L, 428571428571428571L)
    checkResult(TimeFormat(1234L, 999999999999999999L) / 1234L, 1L, 810372771474878L)
    checkResult(TimeFormat(8926414L, 962954926296288974L) / 7233L, 1234L, 123456789012345678L)
    checkResult(TimeFormat(28420679559L, 999999999976987304L) / 23012696L, 1234L, 999999999999999999L)
    checkResult(TimeFormat(1L, 0L) / 1000000000, 0L, 1000000000L)
  }

  test("time format random division") {
    val rand = Random()

    for (i <- 0 until 1_000_000) {
      val t = TimeFormat(rand.nextLong(1000 * 365 * 24 * 60 * 60L), rand.nextLong(1000000000000000000L))
      val scalar = Math.max(1, rand.nextInt(1000000))

      assertResult(0)(t.compareTo(t.*(scalar)./(scalar)))
    }
  }

  test("time format negation") {
    for (s <- -999L until 1000L by 10) {
      for (a <- -999L until 1000L by 10) {
        val t = TimeFormat(s, a)
        assert((t + t.negate()).isZero)
      }
    }
  }

  test("time format equals") {
    val offsets = Array(TimeFormat(200L, 300L), TimeFormat(70L, 1L), TimeFormat(0L, 0L), TimeFormat(-25L, 1L), TimeFormat.DAY, TimeFormat.HOUR, TimeFormat.MICROSECOND)
    for (i <- offsets.indices) {
      for (j <- offsets.indices) {
        if i == j then assert(offsets(i) === offsets(j))
        else assert(offsets(i) != offsets(j))
      }
    }
  }

  test("time format check multiples") {
    for (i <- 1 to 100) do
      checkMultiple(i, TimeFormat.Zero, TimeFormat.Zero)

    checkMultiple(1, TimeFormat.DAY, TimeFormat.DAY)
    checkMultiple(1000, TimeFormat.MILLISECOND, TimeFormat.SECOND)
    checkMultiple(1e6.toInt, TimeFormat.MICROSECOND, TimeFormat.SECOND)
    checkMultiple(1000, TimeFormat.NANOSECOND, TimeFormat.MICROSECOND)
    checkMultiple(1000, TimeFormat.MICROSECOND, TimeFormat.MILLISECOND)
    checkMultiple(60, TimeFormat.SECOND, TimeFormat.MINUTE)
    checkMultiple(60, TimeFormat.MINUTE, TimeFormat.HOUR)
    checkMultiple(24, TimeFormat.HOUR, TimeFormat.DAY)
  }

  def checkMultiple(n: Int, t1: TimeFormat, t2: TimeFormat): Unit = {
    /* if t2 is the nth multiple of t1, then t1 * n = t2, that means t1 * n - t2 = 0 */
    assert(t1.*(n).-(t2).isZero)
  }

  private def checkResult(t: TimeFormat, seconds: Long, attoSeconds: Long): Unit = {
    assertResult(seconds)(t.getSeconds)
    assertResult(attoSeconds)(t.getAttoSeconds)
  }

  test("time format from time unit") {
    import TimeFormat.fromTimeUnit
    import TimeUnit.*

    checkResult(fromTimeUnit(10, DAYS), 864_000L, 0L)
    checkResult(fromTimeUnit(100, HOURS), 360_000L, 0L)
    checkResult(fromTimeUnit(2, MINUTES), 120L, 0L)
    checkResult(fromTimeUnit(5, SECONDS), 5L, 0L)
    checkResult(fromTimeUnit(8, MILLISECONDS), 0L, 800_000_000_000_000_0L)
    checkResult(fromTimeUnit(2, MICROSECONDS), 0L, 2e12.toLong)
    checkResult(fromTimeUnit(7, NANOSECONDS), 0L, 7e9.toLong)
    checkResult(fromTimeUnit(25, PICOSECONDS), 0L, 25e6.toLong)
    checkResult(fromTimeUnit(37, FEMTOSECONDS), 0L, 37e3.toLong)
    checkResult(fromTimeUnit(100, ATTOSECONDS), 0L, 100L)
  }

  test("time format from double with positive second") {
    import TimeFormat.fromDouble

    val d = 123.4567890123456789
    val t = fromDouble(d)

    // adjusted for double precision loss
    checkResult(t, 123L, 456789012345680576L)
    d should be(almostEquals(t.toDouble))
  }

  test("time format from double with negative second") {
    import TimeFormat.fromDouble
    val d = -123.4567890123456789
    val t = fromDouble(d)

    // adjusted for double precision loss
    checkResult(t, -124L, 543210987654319424L)
    d should be(almostEquals(t.toDouble))
  }

  private def checkRoundingResult(precision: Int, actual: TimeFormat, expected: TimeFormat): Unit = {
    assertResult(expected)(actual.getRoundedFormat(precision))
  }

  test("time format rounded format positive") {
    checkRoundingResult(0, TimeFormat(70L, 123456789012345678L), TimeFormat(70L, 0L))
    checkRoundingResult(1, TimeFormat(9L, 123456789012345678L), TimeFormat(9L, 1L))
    checkRoundingResult(2, TimeFormat(70L, 123456789012345678L), TimeFormat(70L, 12L))
    checkRoundingResult(3, TimeFormat(70L, 123456789012345678L), TimeFormat(70L, 123L))
    checkRoundingResult(4, TimeFormat(70L, 123456789012345678L), TimeFormat(70L, 1235L))
    checkRoundingResult(5, TimeFormat(70L, 123456789012345678L), TimeFormat(70L, 12346L))
    checkRoundingResult(6, TimeFormat(70L, 123456789012345678L), TimeFormat(70L, 123457L))
    checkRoundingResult(7, TimeFormat(70L, 123456789012345678L), TimeFormat(70L, 1234568L))
    checkRoundingResult(8, TimeFormat(70L, 123456789012345678L), TimeFormat(70L, 12345679L))
    checkRoundingResult(9, TimeFormat(70L, 123456789012345678L), TimeFormat(70L, 123456789L))
    checkRoundingResult(10, TimeFormat(70L, 123456789012345678L), TimeFormat(70L, 1234567890L))
    checkRoundingResult(11, TimeFormat(70L, 123456789012345678L), TimeFormat(70L, 12345678901L))
    checkRoundingResult(12, TimeFormat(70L, 123456789012345678L), TimeFormat(70L, 123456789012L))
    checkRoundingResult(13, TimeFormat(70L, 123456789012345678L), TimeFormat(70L, 1234567890123L))
    checkRoundingResult(14, TimeFormat(70L, 123456789012345678L), TimeFormat(70L, 12345678901235L))
    checkRoundingResult(15, TimeFormat(70L, 123456789012345678L), TimeFormat(70L, 123456789012346L))
    checkRoundingResult(16, TimeFormat(70L, 123456789012345678L), TimeFormat(70L, 1234567890123457L))
    checkRoundingResult(17, TimeFormat(70L, 123456789012345678L), TimeFormat(70L, 12345678901234568L))
    checkRoundingResult(18, TimeFormat(70L, 123456789012345678L), TimeFormat(70L, 123456789012345678L))
  }

  test("time format rounded format negetive") {
    checkRoundingResult(0, TimeFormat(70L, 123456789012345678L).negate(), TimeFormat(70L, 0L).negate())
    checkRoundingResult(1, TimeFormat(9L, 123456789012345678L).negate(), TimeFormat(9L, 1L).negate())
    checkRoundingResult(2, TimeFormat(70L, 123456789012345678L).negate(), TimeFormat(70L, 12L).negate())
    checkRoundingResult(3, TimeFormat(70L, 123456789012345678L).negate(), TimeFormat(70L, 123L).negate())
    checkRoundingResult(4, TimeFormat(70L, 123456789012345678L).negate(), TimeFormat(70L, 1235L).negate())
    checkRoundingResult(5, TimeFormat(70L, 123456789012345678L).negate(), TimeFormat(70L, 12346L).negate())
    checkRoundingResult(6, TimeFormat(70L, 123456789012345678L).negate(), TimeFormat(70L, 123457L).negate())
    checkRoundingResult(7, TimeFormat(70L, 123456789012345678L).negate(), TimeFormat(70L, 1234568L).negate())
    checkRoundingResult(8, TimeFormat(70L, 123456789012345678L).negate(), TimeFormat(70L, 12345679L).negate())
    checkRoundingResult(9, TimeFormat(70L, 123456789012345678L).negate(), TimeFormat(70L, 123456789L).negate())
    checkRoundingResult(10, TimeFormat(70L, 123456789012345678L).negate(), TimeFormat(70L, 1234567890L).negate())
    checkRoundingResult(11, TimeFormat(70L, 123456789012345678L).negate(), TimeFormat(70L, 12345678901L).negate())
    checkRoundingResult(12, TimeFormat(70L, 123456789012345678L).negate(), TimeFormat(70L, 123456789012L).negate())
    checkRoundingResult(13, TimeFormat(70L, 123456789012345678L).negate(), TimeFormat(70L, 1234567890123L).negate())
    checkRoundingResult(14, TimeFormat(70L, 123456789012345678L).negate(), TimeFormat(70L, 12345678901235L).negate())
    checkRoundingResult(15, TimeFormat(70L, 123456789012345678L).negate(), TimeFormat(70L, 123456789012346L).negate())
    checkRoundingResult(16, TimeFormat(70L, 123456789012345678L).negate(), TimeFormat(70L, 1234567890123457L).negate())
    checkRoundingResult(17, TimeFormat(70L, 123456789012345678L).negate(), TimeFormat(70L, 12345678901234568L).negate())
    checkRoundingResult(18, TimeFormat(70L, 123456789012345678L).negate(), TimeFormat(70L, 123456789012345678L).negate())
  }

  test("small negative fromDouble") {
    val t1 = TimeFormat.fromDouble(-1.0e-17)
    assertResult(-1L)(t1.getSeconds)
    assertResult(999999999999999990L)(t1.getAttoSeconds)
  }
}
