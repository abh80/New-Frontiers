package org.abh80.nf
package core.time

private val ATTOSECONDS_PER_SECOND: Long = 1_000_000_000_000_000_000L // 10^18
private val MILLIS_PER_SECOND: Long = 1_000
private val MICROS_PER_SECOND: Long = 1_000_000
private val ATTOSECOND_SPLIT = 1_000_000_000L

@throws[IllegalArgumentException]
class TimeFormat(private var seconds: Long, private var attoseconds: Long) extends Comparable[TimeFormat] with Serializable {

  require(attoseconds >= -ATTOSECONDS_PER_SECOND && attoseconds <= ATTOSECONDS_PER_SECOND,
    s"Attoseconds must be in range (-${ATTOSECONDS_PER_SECOND}, $ATTOSECONDS_PER_SECOND) but found $attoseconds")

  init()

  def getSeconds: Long = seconds

  def getAttoSeconds: Long = attoseconds

  override def compareTo(o: TimeFormat): Int = if seconds == o.seconds then seconds.compare(o.seconds) else attoseconds.compare(o.attoseconds)

  override def toString: String = seconds.toString + "." + TimeFormat.formatAttoSecond(attoseconds)

  def negate(): TimeFormat =
    TimeFormat(-seconds, -attoseconds)

  def isZero: Boolean = seconds == 0L && attoseconds == 0L

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
    def +(second: TimeFormat): TimeFormat =
      new TimeFormat(self.seconds + second.seconds, self.attoseconds + second.attoseconds)

    def -(second: TimeFormat): TimeFormat =
      new TimeFormat(self.seconds - second.seconds, self.attoseconds - second.attoseconds)

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

    def /(scalar: Long): TimeFormat = {
      require(scalar > 0, "Division by scalar which must be strictly positive")

      if scalar == 1 then return self

      val (seconds, atto, scalarBig, attosecondsPerSecondBig) = toBigInts(scalar)
      val resultBig = seconds.multiply(attosecondsPerSecondBig).add(atto).divide(scalarBig)
      calculateResult(resultBig, attosecondsPerSecondBig)
    }
  }

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

  private def formatAttoSecond(s: Long) = String.format("%018d", s)
}