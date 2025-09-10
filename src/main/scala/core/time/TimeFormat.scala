package org.abh80.nf
package core.time

import scala.math.{round, rint}
import scala.math.BigDecimal.RoundingMode
import scala.math.Ordered.orderingToOrdered
import scala.util.hashing.MurmurHash3

private val ATTOSECONDS_PER_SECOND: Long = 1_000_000_000_000_000_000L // 10^18
private val MILLIS_PER_SECOND: Long = 1_000
private val MICROS_PER_SECOND: Long = 1_000_000
private val ATTOSECOND_SPLIT = 1_000_000_000L

/**
 * Represents a time format with second and attosecond precision.
 *
 * This class allows representing time values with a high degree of precision,
 * using seconds and attoseconds (10&#94;-18 seconds) as the units.
 * It provides functionalities for basic arithmetic operations,
 * comparisons, and conversions to and from other time units.
 *
 * @param seconds     The number of seconds. Can be positive, negative, or zero.
 * @param attoseconds The number of attoseconds. Can be positive, negative, or zero.
 *                    Must be in the range (-10&#94;18, 10&#94;18).
 * @throws IllegalArgumentException if attoseconds is outside the allowed range.
 *
 *                                  Example usage:
 * {{{
 *   val time1 = new TimeFormat(1, 500000000000000000L) // 1.5 seconds
 *   val time2 = new TimeFormat(0, 750000000000000000L) // 0.75 seconds
 *
 *   val sum = time1 + time2 // 2.25 seconds
 *   val difference = time1 - time2 // 0.75 seconds
 *
 *   println(s"Sum: $sum")
 *   println(s"Difference: $difference")
 * }}}
 */
@throws[IllegalArgumentException]
class TimeFormat(private var seconds: Long, private var attoseconds: Long) extends Comparable[TimeFormat] with Serializable {

  require(attoseconds >= Long.MinValue && attoseconds <= Long.MaxValue,
    s"Attoseconds must be in range (${Long.MinValue}, ${Long.MaxValue}) but found $attoseconds")

  init()

  /** Compares this TimeFormat instance with another TimeFormat instance.
   *
   * @param o The TimeFormat instance to compare with
   * @return An integer value:
   *         - Negative if this instance is less than the other
   *         - Zero if they are equal
   *         - Positive if this instance is greater than the other
   */
  override def compareTo(o: TimeFormat): Int = if seconds != o.seconds then seconds.compare(o.seconds) else attoseconds.compare(o.attoseconds)

  /**
   * Converts current time format to double, "seconds[.]attoseconds"
   *
   * @return a double value
   */
  def toDouble: Double = seconds + (attoseconds.toDouble / ATTOSECONDS_PER_SECOND)

  /** Returns a string representation of the time format in the form "seconds.attoseconds"
   * where attoseconds is padded with leading zeros to 18 digits.
   *
   * @return A string representation of the time value
   */
  override def toString: String = seconds.toString + "." + TimeFormat.formatAttoSecond(attoseconds)

  /** Creates a new TimeFormat instance with the negated values of seconds and attoseconds.
   *
   * @return A new TimeFormat instance with negated values
   */
  def negate(): TimeFormat =
    TimeFormat(-seconds, -attoseconds)

  /** Checks if this TimeFormat represents zero time (both seconds and attoseconds are 0).
   *
   * @return true if the time value is zero, false otherwise
   */
  def isZero: Boolean = seconds == 0L && attoseconds == 0L

  /** Copy constructor that creates a new TimeFormat instance from an existing one.
   *
   * @param tf The TimeFormat instance to copy
   */
  def this(tf: TimeFormat) =
    this(tf.getSeconds, tf.getAttoSeconds)

  def getRoundedFormat(precision: Int): TimeFormat = {
    if (attoseconds < 0) return TimeFormat.Zero

    // Compute the value as a BigDecimal in seconds
    val value = BigDecimal(seconds) + BigDecimal(attoseconds) / BigDecimal("1e18")
    // Round the value to the desired precision (number of decimal places)
    val roundedValue = value.setScale(precision, RoundingMode.HALF_UP)

    // Extract whole seconds part
    val secondsRounded: Long = roundedValue.setScale(0, RoundingMode.DOWN).toLong

    // Extract fractional part, then scale to attoseconds at the desired precision
    val scaleFactor = BigDecimal(10).pow(precision)
    val attosecBig = (roundedValue - BigDecimal(secondsRounded)) * scaleFactor
    val attosecondsRounded: Long = attosecBig.setScale(0, RoundingMode.HALF_UP).toLong

    TimeFormat(secondsRounded, attosecondsRounded)
  }

  override def hashCode(): Int = MurmurHash3.productHash((seconds, attoseconds))

  override def equals(obj: Any): Boolean =
    obj match {
      case s: TimeFormat => s.getSeconds == seconds && s.getAttoSeconds == attoseconds
      case _ => false
    }

  /** Gets the seconds component of the time value.
   *
   * @return The number of seconds
   */
  def getSeconds: Long = seconds

  /** Gets the attoseconds component of the time value.
   *
   * @return The number of attoseconds
   */
  def getAttoSeconds: Long = attoseconds

  private def init(): Unit =
    val qAtto = attoseconds / ATTOSECONDS_PER_SECOND
    val rAtto = attoseconds - qAtto * ATTOSECONDS_PER_SECOND
    if (rAtto < 0L) {
      this.seconds = seconds + qAtto - 1L
      this.attoseconds = ATTOSECONDS_PER_SECOND + rAtto
    } else {
      this.seconds = seconds + qAtto
      this.attoseconds = rAtto
    }
}

object TimeFormat {

  val Zero = new TimeFormat(0L, 0L)
  val ATTOSECOND = new TimeFormat(0L, 1L)
  val FEMTOSECOND = new TimeFormat(0L, 1_000L)
  val PICOSECOND = new TimeFormat(0L, 1_000_000L)
  val NANOSECOND = new TimeFormat(0L, 1_000_000_000L)
  val MICROSECOND = new TimeFormat(0L, 1_000_000_000_000L)
  val MILLISECOND = new TimeFormat(0L, 1_000_000_000_000_000L)
  val SECOND = new TimeFormat(1L, 0L)
  val MINUTE = new TimeFormat(60L, 0L)
  val HOUR = new TimeFormat(3_600L, 0L)
  val DAY = new TimeFormat(86_400L, 0L)


  implicit class BinOp(self: TimeFormat) {
    /** Adds two TimeFormat instances.
     *
     * @param second The TimeFormat instance to add
     * @return A new TimeFormat instance representing the sum
     */
    def +(second: TimeFormat): TimeFormat =
      new TimeFormat(self.seconds + second.seconds, self.attoseconds + second.attoseconds)

    /** Subtracts one TimeFormat instance from another.
     *
     * @param second The TimeFormat instance to subtract
     * @return A new TimeFormat instance representing the difference
     */
    def -(second: TimeFormat): TimeFormat =
      new TimeFormat(self.seconds - second.seconds, self.attoseconds - second.attoseconds)

    /** Multiplies the time value by a scalar.
     *
     * @param scalar The non-negative scalar value to multiply by
     * @return A new TimeFormat instance representing the product
     * @throws IllegalArgumentException if scalar is negative
     */
    def *(scalar: Long): TimeFormat = {
      require(scalar >= 0, "Multiplication by scalar cannot be negative")

      if scalar == 0 then return new TimeFormat(0L, 0L)
      if scalar == 1 then return self

      val (seconds, atto, scalarBig, attosecondsPerSecondBig) = toBigInts(scalar)
      val resultBig = seconds.multiply(attosecondsPerSecondBig).add(atto).multiply(scalarBig)
      calculateResult(resultBig, attosecondsPerSecondBig)
    }

    @throws[IllegalArgumentException]
    @throws[ArithmeticException]
    private def toBigInts(scalar: Long) = {
      import java.math.BigInteger
      val seconds = BigInteger.valueOf(self.seconds)
      val atto = BigInteger.valueOf(self.attoseconds)
      val scalarBig = BigInteger.valueOf(scalar)
      val attosecondsPerSecondBig = BigInteger.valueOf(ATTOSECONDS_PER_SECOND)
      (seconds, atto, scalarBig, attosecondsPerSecondBig)
    }

    private def calculateResult(resultBig: java.math.BigInteger, attosecondsPerSecondBig: java.math.BigInteger) = {
      val resSeconds = resultBig.divide(attosecondsPerSecondBig)
      val resAttoseconds = resultBig.remainder(attosecondsPerSecondBig)
      new TimeFormat(resSeconds.longValueExact(), resAttoseconds.longValueExact())
    }

    /** Divides the time value by a scalar.
     *
     * @param scalar The positive scalar value to divide by
     * @return A new TimeFormat instance representing the quotient
     * @throws IllegalArgumentException if scalar is not positive
     */
    def /(scalar: Long): TimeFormat = {
      require(scalar > 0, "Division by scalar which must be strictly positive")

      if scalar == 1 then return self

      val (seconds, atto, scalarBig, attosecondsPerSecondBig) = toBigInts(scalar)
      val resultBig = seconds.multiply(attosecondsPerSecondBig).add(atto).divide(scalarBig)
      calculateResult(resultBig, attosecondsPerSecondBig)
    }
  }

  /** Converts a time value from a specific TimeUnit to TimeFormat.
   *
   * @param value The time value to convert
   * @param unit  The TimeUnit of the input value
   * @return A new TimeFormat instance representing the converted time
   */
  def fromTimeUnit(value: Long, unit: TimeUnit): TimeFormat =
    unit match {
      case TimeUnit.DAYS => DAY * value
      case TimeUnit.HOURS => HOUR * value
      case TimeUnit.MINUTES => MINUTE * value
      case TimeUnit.SECONDS => SECOND * value
      case TimeUnit.MILLISECONDS => MILLISECOND * value
      case TimeUnit.MICROSECONDS => MICROSECOND * value
      case TimeUnit.NANOSECONDS => NANOSECOND * value
      case TimeUnit.PICOSECONDS => PICOSECOND * value
      case TimeUnit.FEMTOSECONDS => FEMTOSECOND * value
      case TimeUnit.ATTOSECONDS => ATTOSECOND * value
    }

  @throws[IllegalArgumentException]
  def fromDouble(seconds: Double): TimeFormat = {
    require(Double.NaN != seconds, "Input should not be a type of NaN")
    require(Double.MaxValue >= seconds && Double.MinValue <= seconds, "Input seconds is not in range of Double")

    val roundSeconds = rint(seconds)
    val frac = seconds - roundSeconds


    if frac < 0 then
      TimeFormat(roundSeconds.toLong - 1L, round(frac * ATTOSECONDS_PER_SECOND) + ATTOSECONDS_PER_SECOND)
    else TimeFormat(roundSeconds.toLong, round(frac * ATTOSECONDS_PER_SECOND))
  }

  /** Formats attoseconds as a string with leading zeros to 18 digits.
   *
   * @param s The attoseconds value to format
   * @return A string representation of attoseconds padded to 18 digits
   */
  private def formatAttoSecond(s: Long) = String.format("%018d", s)
}