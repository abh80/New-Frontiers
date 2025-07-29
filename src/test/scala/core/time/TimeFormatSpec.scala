package org.abh80.nf
package core.time

import org.scalatest.funsuite.AnyFunSuite

import scala.util.Random

class TimeFormatSpec extends AnyFunSuite {
  test("time offset addition") {
    checkResult(TimeFormat(24L, 940L) + TimeFormat(36L, 7400L), 60L, 8340L)
    checkResult(TimeFormat(24L, 940L) + TimeFormat(-20L, -1000L), 3L, 999999999999999940L)
  }

  test("time offset subtraction") {
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

  test("time offset constants") {
    checkResult(TimeFormat.MICROSECOND, 0L, 1_000_000_000_000L)
    checkResult(TimeFormat.MILLISECOND, 0L, 1_000_000_000_000_000L)
    checkResult(TimeFormat.SECOND, 1L, 0L)
    checkResult(TimeFormat.MINUTE, 60L, 0L)
    checkResult(TimeFormat.HOUR, 3600L, 0L)
    checkResult(TimeFormat.DAY, 86400L, 0L)
  }

  test("time offset multiplication") {
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

  test("time offset division") {
    assertThrows[IllegalArgumentException](TimeFormat(1L, 1L) / 0L)

    checkResult(TimeFormat(1234L, 123456789012345678L) / 1L, 1234L, 123456789012345678L)
    checkResult(TimeFormat(1234L, 999999999999999999L) / 7L, 176L, 428571428571428571L)
    checkResult(TimeFormat(1234L, 999999999999999999L) / 1234L, 1L, 810372771474878L)
    checkResult(TimeFormat(8926414L, 962954926296288974L) / 7233L, 1234L, 123456789012345678L)
    checkResult(TimeFormat(28420679559L, 999999999976987304L) / 23012696L, 1234L, 999999999999999999L)
    checkResult(TimeFormat(1L, 0L) / 1000000000, 0L, 1000000000L)
  }

  test("time offset random division") {
    val rand = Random()

    for (i <- 0 until 1_000_000) {
      val t = TimeFormat(rand.nextLong(1000 * 365 * 24 * 60 * 60L), rand.nextLong(1000000000000000000L))
      val scalar = Math.max(1, rand.nextInt(1000000))

      assertResult(0)(t.compareTo(t.*(scalar)./(scalar)))
    }
  }

  test("time offset negation") {
    for (s <- -999L until 1000L by 10) {
      for (a <- -999L until 1000L by 10) {
        val t = TimeFormat(s, a)
        assert((t + t.negate()).isZero)
      }
    }
  }

  test("time offset equals") {
    val offsets = Array(TimeFormat(200L, 300L), TimeFormat(70L, 1L), TimeFormat(0L, 0L), TimeFormat(-25L, 1L), TimeFormat.DAY, TimeFormat.HOUR, TimeFormat.MICROSECOND)
    for (i <- offsets.indices) {
      for (j <- offsets.indices) {
        if i == j then assert(offsets(i) === offsets(j))
        else assert(offsets(i) != offsets(j))
      }
    }
  }

  test("time offset check multiples") {
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

  test("out of range") {
    assertThrows[IllegalArgumentException](TimeFormat(0L, 1e25.toLong))
    assertThrows[IllegalArgumentException](TimeFormat(0L, -1e25.toLong))
    assertThrows[IllegalArgumentException](TimeFormat(0L, 1e19.toLong))
  }
}
