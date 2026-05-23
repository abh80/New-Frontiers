package bench.precision

import bench.support.PrecisionResult
import org.abh80.nf.core.math.calculus.FiniteDifferencesDifferentiator
import org.hipparchus.analysis.UnivariateFunction
import org.hipparchus.analysis.differentiation.{DSFactory, FiniteDifferencesDifferentiator as HFDD}

/** Error of the numeric first derivative vs the analytic derivative. */
object DifferentiatorPrecisionChecks:
  private val cat = "Differentiator"
  private val nbPoints = 5
  private val step = 1e-3
  private val x = 0.7
  private val dsf = new DSFactory(1, 1)

  private case class Case(name: String, f: Double => Double, dfTruth: Double)
  private val cases = Seq(
    Case("sin",   math.sin, math.cos(x)),
    Case("exp",   math.exp, math.exp(x)),
    Case("recip", t => 1.0 / (1.0 + t), -1.0 / ((1.0 + x) * (1.0 + x)))
  )

  def run(): Seq[PrecisionResult] =
    cases.flatMap { c =>
      val nf = new FiniteDifferencesDifferentiator(nbPoints, step).differentiate(c.f)(x)(1)
      val uf: UnivariateFunction = (t: Double) => c.f(t)
      val ok = new HFDD(nbPoints, step).differentiate(uf).value(dsf.variable(0, x)).getPartialDerivative(1)
      Seq(
        PrecisionResult(cat, c.name, "nf", math.abs(nf - c.dfTruth)),
        PrecisionResult(cat, c.name, "ok", math.abs(ok - c.dfTruth))
      )
    }
