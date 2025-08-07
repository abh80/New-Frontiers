package org.abh80.nf
package core.time

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class TimeSpec extends AnyFunSuite with Matchers {
  test("hour less than 0 throws IllegalArgumentException") {
    assertThrows[IllegalArgumentException] {
      new Time(-1, 0, TimeFormat.fromDouble(0.0), 0)
    }
  }

  test("hour equal to 24 throws IllegalArgumentException") {
    assertThrows[IllegalArgumentException] {
      new Time(24, 0, TimeFormat.fromDouble(0.0), 0)
    }
  }

  test("minute less than 0 throws IllegalArgumentException") {
    assertThrows[IllegalArgumentException] {
      new Time(0, -1, TimeFormat.fromDouble(0.0), 0)
    }
  }

  test("minute equal to 60 throws IllegalArgumentException") {
    assertThrows[IllegalArgumentException] {
      new Time(0, 60, TimeFormat.fromDouble(0.0), 0)
    }
  }

  test("seconds less than 0 throws IllegalArgumentException") {
    assertThrows[IllegalArgumentException] {
      new Time(0, 0, TimeFormat.fromDouble(-0.1), 0)
    }
  }

  test("seconds greater than 61 throws IllegalArgumentException") {
    assertThrows[IllegalArgumentException] {
      new Time(0, 0, TimeFormat.fromDouble(62.0), 0)
    }
  }

  test("valid boundary values do NOT throw") {
    noException should be thrownBy {
      new Time(0, 0, TimeFormat.fromDouble(0.0), 0)
      new Time(23, 59, TimeFormat.fromDouble(61.0), 0)
    }
  }

  test("in range") {
    checkResult(Time(5, 5, 5), 5, 5, 5.0)
    Time(0.0).getSecondsInDay should be(0.0)
    checkResult(Time(5, 5, 60.9999), 5, 5, 60.9999)
    Time(24_000.0).getSecondsInUTCDay should be(24_000.0)
  }

  test("with utc offset") {
    val t = Time(5, 43, 0, 70)
    t.getSecondsInDay should be(20_580.0)
    t.getSecondsInUTCDay should be(16_380.0)
    t.getUtcOffset should be(70)
  }

  test("values") {
    Time(0, 0, 0).getSecondsInDay shouldEqual 0.0
    Time(23, 59, 59.9).getSecondsInDay shouldEqual 86_399.9
  }

  test("to string") {
    // Basic formatting with UTC offset
    val time1 = createTime(hour = 14, minute = 30, seconds = 45, utcOffset = 0)
    time1.toString shouldBe "14:30:45.000+00:00"

    val time2 = createTime(hour = 9, minute = 15, seconds = 30, utcOffset = 120) // +2 hours
    time2.toString shouldBe "09:15:30.000+02:00"

    // Negative UTC offsets
    val time3 = createTime(hour = 16, minute = 45, seconds = 20, utcOffset = -300) // -5 hours
    time3.toString shouldBe "16:45:20.000-05:00"

    // Fractional seconds
    val time4 = createTime(hour = 12, minute = 0, seconds = 15, attoSeconds = 123456789012345678L, utcOffset = 60)
    time4.toString shouldBe "12:00:15.123456789012345678+01:00"

    // Single digit padding
    val time5 = createTime(hour = 1, minute = 2, seconds = 3, utcOffset = -30)
    time5.toString shouldBe "01:02:03.000-00:30"

    // Edge cases for UTC offset
    val utcTime = createTime(hour = 12, minute = 0, seconds = 0, utcOffset = 0)
    utcTime.toString shouldBe "12:00:00.000+00:00"

    val maxPositive = createTime(hour = 12, minute = 0, seconds = 0, utcOffset = 840) // +14:00
    maxPositive.toString shouldBe "12:00:00.000+14:00"

    val maxNegative = createTime(hour = 12, minute = 0, seconds = 0, utcOffset = -720) // -12:00
    maxNegative.toString shouldBe "12:00:00.000-12:00"

    // Non-standard minute offsets
    val time30min = createTime(hour = 10, minute = 30, seconds = 45, utcOffset = 330) // +5:30
    time30min.toString shouldBe "10:30:45.000+05:30"

    val timeNeg45min = createTime(hour = 8, minute = 15, seconds = 0, utcOffset = -45, attoSeconds = 32281798015773L) // -0:45
    timeNeg45min.toString shouldBe "08:15:00.000032281798015773-00:45"

    createTime(hour = 4, minute = 24, seconds = 10, attoSeconds = 999999999999999999L).toString shouldBe "04:24:10.999999999999999999+00:00"

    createTime(hour = 4, minute = 24, seconds = 10, attoSeconds = 999999999999999000L).toString shouldBe "04:24:10.999999999999999+00:00"
  }

  test("compare to") {
    Time(4, 35, 29.999).compareTo(Time(4, 35, 29.999)) shouldEqual 0
    Time(4, 35, 29.998).compareTo(Time(4, 35, 29.999)) should be < 0
    Time(5, 35, 17, 40).compareTo(Time(5, 35, 17, 70)) should be > 0
  }

  test("invalid leap second") {
    assertThrows[IllegalArgumentException] {
      Time(86401.0)
    }
  }

  test("leap second") {
    checkResult(Time(86400.0), 23, 59, 60)
  }

  test("equality") {
    Time(23, 59, 59.99999) shouldEqual Time(23, 59, 59.99999)
    Time(23, 58, 45) should not equal Time(23, 59, 45)
    Time(10, 10, 10, 45) should not equal Time(10, 10, 10, 50)
    Time(10, 10, 10, 10) shouldEqual Time(10, 10, 10, 10)
  }

  test("ISO8601 string") {
    Time(10, 25, 43.12345).toISO8601String(1) shouldEqual "10:25:43.1"
    Time(10, 25, 43.1256).toISO8601String(2) shouldEqual "10:25:43.13"
  }

  private def createTime(hour: Int, minute: Int, seconds: Int, attoSeconds: Long = 0L, utcOffset: Int = 0): Time = {
    Time(hour, minute, TimeFormat(seconds, attoSeconds), utcOffset)
  }

  private def checkResult(time: Time, hour: Int, min: Int, sec: Double): Unit = {
    time.getHour should be(hour)
    time.getMinute should be(min)
    time.getSeconds should be(sec)
  }
}
