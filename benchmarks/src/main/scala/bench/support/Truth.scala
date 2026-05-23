package bench.support

import org.abh80.nf.core.time.AbsoluteTime

/** Independent truth oracle: defined physical/time constants and exact
 *  BigDecimal extraction of NF's internal time representation. */
object Truth:
  /** TT - TAI, a defined constant (seconds). */
  val TT_MINUS_TAI: Double = 32.184

  /** TAI - UTC since the 2017-01-01 leap second (IERS); stable through today. */
  val TAI_MINUS_UTC_SINCE_2017: Double = 37.0

  /** GPS - TAI, a defined constant (seconds). */
  val GPS_MINUS_TAI: Double = -19.0

  private val ATTOS_PER_SECOND: BigDecimal = BigDecimal(10).pow(18)

  /** NF stores time as seconds + attoseconds from J2000; reconstruct it exactly. */
  def nfExactSeconds(t: AbsoluteTime): BigDecimal =
    BigDecimal(t.getSeconds) + BigDecimal(t.getAttoSeconds) / ATTOS_PER_SECOND

  /** Absolute error of a Double against a BigDecimal truth. */
  def absError(actual: Double, truth: BigDecimal): Double =
    (BigDecimal(actual) - truth).abs.toDouble

  /** Absolute error of a BigDecimal actual against a BigDecimal truth. */
  def absError(actual: BigDecimal, truth: BigDecimal): Double =
    (actual - truth).abs.toDouble
