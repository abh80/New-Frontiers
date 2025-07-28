package org.abh80.nf
package core.time

import org.scalatest.funsuite.AnyFunSuite

import scala.util.Random

class TimeOffsetSpec extends AnyFunSuite {
  test("time offset addition") {
    checkResult(TimeOffset(24L, 940L) + TimeOffset(36L, 7400L), 60L, 8340L)
    checkResult(TimeOffset(24L, 940L) + TimeOffset(-20L, -1000L), 3L, 999999999999999940L)
  }

  test("time offset subtraction") {
    checkResult(TimeOffset(1234L, 7028L) - TimeOffset(781L, 1043L), 453L, 5985L)
    checkResult(TimeOffset(1L, 0L) - TimeOffset(2L, 1L), -2L, 999999999999999999L)
  }

  test("toString value") {
    assertResult("0.000000000000000000")(TimeOffset.Zero.toString)
    assertResult("0.000001000000000000")(TimeOffset.MICROSECOND.toString)
    assertResult("0.001000000000000000")(TimeOffset.MILLISECOND.toString)
    assertResult("1.000000000000000000")(TimeOffset.SECOND.toString)
    assertResult("60.000000000000000000")(TimeOffset.MINUTE.toString)
    assertResult("3600.000000000000000000")(TimeOffset.HOUR.toString)
    assertResult("86400.000000000000000000")(TimeOffset.DAY.toString)
  }

  test("time offset constants") {
    checkResult(TimeOffset.MICROSECOND, 0L, 1_000_000_000_000L)
    checkResult(TimeOffset.MILLISECOND, 0L, 1_000_000_000_000_000L)
    checkResult(TimeOffset.SECOND, 1L, 0L)
    checkResult(TimeOffset.MINUTE, 60L, 0L)
    checkResult(TimeOffset.HOUR, 3600L, 0L)
    checkResult(TimeOffset.DAY, 86400L, 0L)
  }

  test("time offset multiplication") {
    assertThrows[IllegalArgumentException](TimeOffset(1L, 1L) * -1) // scalar cannot be negative

    checkResult(TimeOffset(1L, 7L) * 0L, 0L, 0L)
    checkResult(TimeOffset(1L, 7L) * 1L, 1L, 7L)
    checkResult(TimeOffset(1L, 75L) * 5L, 5L, 375L)
    checkResult(TimeOffset(1234L, 123456789012345678L).*(7233L), 8926414L, 962954926296288974L)
    checkResult(TimeOffset(1234L, 999999999999999999L).*(23012696L), 28420679559L, 999999999976987304L)
    checkResult(TimeOffset(1234L, 999999999999999999L).*(123456789012L), 152469134429819L, 999999876543210988L)

    assertThrows[ArithmeticException](TimeOffset(10000000000L, 1L) * 123456789012L) //overflow error
    assertThrows[ArithmeticException](TimeOffset(922382683L, 717054400620018329L) * 1573105907129L) // overflow error

  }

  test("time offset division") {
    assertThrows[IllegalArgumentException](TimeOffset(1L, 1L) / 0L)

    checkResult(TimeOffset(1234L, 123456789012345678L) / 1L, 1234L, 123456789012345678L)
    checkResult(TimeOffset(1234L, 999999999999999999L) / 7L, 176L, 428571428571428571L)
    checkResult(TimeOffset(1234L, 999999999999999999L) / 1234L, 1L, 810372771474878L)
    checkResult(TimeOffset(8926414L, 962954926296288974L) / 7233L, 1234L, 123456789012345678L)
    checkResult(TimeOffset(28420679559L, 999999999976987304L) / 23012696L, 1234L, 999999999999999999L)
    checkResult(TimeOffset(1L, 0L) / 1000000000, 0L, 1000000000L)
  }

  test("time offset random division") {
    val rand = Random()

    for (i <- 0 until 1_000_000) {
      val t = TimeOffset(rand.nextLong(1000 * 365 * 24 * 60 * 60L), rand.nextLong(1000000000000000000L))
      val scalar = Math.max(1, rand.nextInt(1000000))

      assertResult(0)(t.compareTo(t.*(scalar)./(scalar)))
    }
  }

  test("time offset negation") {
    for (s <- -999L until 1000L by 10) {
      for (a <- -999L until 1000L by 10) {
        val t = TimeOffset(s, a)
        assert((t + t.negate()).isZero)
      }
    }
  }

  test("time offset equals") {
    val offsets = Array(TimeOffset(200L, 300L), TimeOffset(70L, 1L), TimeOffset(0L, 0L), TimeOffset(-25L, 1L), TimeOffset.DAY, TimeOffset.HOUR, TimeOffset.MICROSECOND)
    for (i <- offsets.indices) {
      for (j <- offsets.indices) {
        if i == j then assert(offsets(i) === offsets(j))
        else assert(offsets(i) != offsets(j))
      }
    }
  }

  test("time offset check multiples") {
    for (i <- 1 to 100) do
      checkMultiple(i, TimeOffset.Zero, TimeOffset.Zero)

    checkMultiple(1, TimeOffset.DAY, TimeOffset.DAY)
    checkMultiple(1000, TimeOffset.MILLISECOND, TimeOffset.SECOND)
    checkMultiple(1e6.toInt, TimeOffset.MICROSECOND, TimeOffset.SECOND)
    checkMultiple(1000, TimeOffset.NANOSECOND, TimeOffset.MICROSECOND)
    checkMultiple(1000, TimeOffset.MICROSECOND, TimeOffset.MILLISECOND)
    checkMultiple(60, TimeOffset.SECOND, TimeOffset.MINUTE)
    checkMultiple(60, TimeOffset.MINUTE, TimeOffset.HOUR)
    checkMultiple(24, TimeOffset.HOUR, TimeOffset.DAY)
  }

  def checkMultiple(n: Int, t1: TimeOffset, t2: TimeOffset): Unit = {
    /* if t2 is the nth multiple of t1, then t1 * n = t2, that means t1 * n - t2 = 0 */
    assert(t1.*(n).-(t2).isZero)
  }

  private def checkResult(t: TimeOffset, seconds: Long, attoSeconds: Long): Unit = {
    assertResult(seconds)(t.getSeconds)
    assertResult(attoSeconds)(t.getAttoSeconds)
  }

  test("out of range") {
    assertThrows[IllegalArgumentException](TimeOffset(0L, 1e25.toLong))
    assertThrows[IllegalArgumentException](TimeOffset(0L, -1e25.toLong))
    assertThrows[IllegalArgumentException](TimeOffset(0L, 1e19.toLong))
  }
}
