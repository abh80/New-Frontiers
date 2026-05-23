package bench.calculus

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalactic.Tolerance.*

import org.abh80.nf.core.math.calculus.FiniteDifferencesDifferentiator
import org.hipparchus.analysis.UnivariateFunction
import org.hipparchus.analysis.differentiation.{DSFactory, FiniteDifferencesDifferentiator as HFDD}

class DifferentiatorEquivalenceSpec extends AnyFlatSpec with Matchers:
  private val nbPoints = 5
  private val step = 1e-3
  private val x = 0.7
  private val tol = 1e-7

  "NF FiniteDifferencesDifferentiator" should "match Hipparchus on d/dx sin(x)" in {
    val nf = new FiniteDifferencesDifferentiator(nbPoints, step)
    val nfDeriv = nf.differentiate((t: Double) => math.sin(t))(x)(1)

    val hf = new HFDD(nbPoints, step)
    val uf: UnivariateFunction = (t: Double) => math.sin(t)
    val dsf = new DSFactory(1, 1)
    val okDeriv = hf.differentiate(uf).value(dsf.variable(0, x)).getPartialDerivative(1)

    nfDeriv shouldBe (okDeriv +- tol)
    nfDeriv shouldBe (math.cos(x) +- tol) // analytic truth
  }
