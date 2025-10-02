package org.abh80.nf
package core.math.calculus

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class FiniteDifferencesDifferentiatorSpec extends AnyFlatSpec with Matchers {

  behavior of "FiniteDifferencesDifferentiator construction"

  it should "create a differentiator with valid parameters" in {
    val diff = new FiniteDifferencesDifferentiator(5, 0.01)
    diff.nbPoints shouldBe 5
    diff.stepSize shouldBe 0.01
    diff.tMin shouldBe Double.NegativeInfinity
    diff.tMax shouldBe Double.PositiveInfinity
  }

  it should "create a differentiator with bounded domain" in {
    val diff = new FiniteDifferencesDifferentiator(5, 0.01, -10.0, 10.0)
    diff.nbPoints shouldBe 5
    diff.stepSize shouldBe 0.01
    diff.tMin shouldBe -10.0
    diff.tMax shouldBe 10.0
  }

  it should "throw IllegalArgumentException when nbPoints <= 1" in {
    an[IllegalArgumentException] should be thrownBy {
      new FiniteDifferencesDifferentiator(1, 0.01)
    }
  }

  it should "throw IllegalArgumentException when stepSize <= 0" in {
    an[IllegalArgumentException] should be thrownBy {
      new FiniteDifferencesDifferentiator(5, 0.0)
    }
    an[IllegalArgumentException] should be thrownBy {
      new FiniteDifferencesDifferentiator(5, -0.01)
    }
  }

  it should "throw IllegalArgumentException when sample span is too large for bounds" in {
    an[IllegalArgumentException] should be thrownBy {
      new FiniteDifferencesDifferentiator(10, 1.0, 0.0, 5.0)
    }
  }

  behavior of "differentiate method with polynomial functions"

  it should "compute first derivative of linear function correctly" in {
    val diff = new FiniteDifferencesDifferentiator(3, 0.01)
    val f = (x: Double) => 2.0 * x + 3.0 // f'(x) = 2
    val derivative = diff.differentiate(f)

    derivative(0.0)(1) shouldBe 2.0 +- 1e-10
    derivative(5.0)(1) shouldBe 2.0 +- 1e-10
    derivative(-3.0)(1) shouldBe 2.0 +- 1e-10
  }

  it should "compute first derivative of quadratic function correctly" in {
    val diff = new FiniteDifferencesDifferentiator(5, 0.001)
    val f = (x: Double) => x * x // f'(x) = 2x
    val derivative = diff.differentiate(f)

    derivative(1.0)(1) shouldBe 2.0 +- 1e-6
    derivative(3.0)(1) shouldBe 6.0 +- 1e-6
    derivative(-2.0)(1) shouldBe -4.0 +- 1e-6
  }

  it should "compute second derivative of quadratic function correctly" in {
    val diff = new FiniteDifferencesDifferentiator(5, 0.001)
    val f = (x: Double) => x * x // f''(x) = 2
    val derivative = diff.differentiate(f)

    derivative(0.0)(2) shouldBe 2.0 +- 1e-5
    derivative(5.0)(2) shouldBe 2.0 +- 1e-5
  }

  it should "compute derivatives of cubic function correctly" in {
    val diff = new FiniteDifferencesDifferentiator(7, 0.0001)
    val f = (x: Double) => x * x * x // f'(x) = 3x^2, f''(x) = 6x, f'''(x) = 6
    val derivative = diff.differentiate(f)

    derivative(2.0)(1) shouldBe 12.0 +- 1e-4 // 3 * 2^2 = 12
    derivative(2.0)(2) shouldBe 12.0 +- 1e-3 // 6 * 2 = 12
    derivative(2.0)(3) shouldBe 6.0 +- 1e-2 // 6
  }

  it should "compute zeroth derivative (function value) correctly" in {
    val diff = new FiniteDifferencesDifferentiator(5, 0.01)
    val f = (x: Double) => x * x + 2.0 * x + 1.0
    val derivative = diff.differentiate(f)

    derivative(3.0)(0) shouldBe 16.0 +- 1e-8
    derivative(0.0)(0) shouldBe 1.0 +- 1e-8
  }

  behavior of "differentiate method with transcendental functions"

  it should "compute derivative of sin(x) correctly" in {
    val diff = new FiniteDifferencesDifferentiator(7, 0.0001)
    val f = (x: Double) => math.sin(x) // f'(x) = cos(x)
    val derivative = diff.differentiate(f)

    derivative(0.0)(1) shouldBe math.cos(0.0) +- 1e-6
    derivative(math.Pi / 4)(1) shouldBe math.cos(math.Pi / 4) +- 1e-6
    derivative(math.Pi / 2)(1) shouldBe math.cos(math.Pi / 2) +- 1e-6
  }

  it should "compute derivative of exp(x) correctly" in {
    val diff = new FiniteDifferencesDifferentiator(5, 0.001)
    val f = (x: Double) => math.exp(x) // f'(x) = exp(x)
    val derivative = diff.differentiate(f)

    derivative(0.0)(1) shouldBe math.exp(0.0) +- 1e-5
    derivative(1.0)(1) shouldBe math.exp(1.0) +- 1e-4
    derivative(2.0)(1) shouldBe math.exp(2.0) +- 1e-3
  }

  behavior of "differentiate method error handling"

  it should "throw IllegalArgumentException for negative order" in {
    val diff = new FiniteDifferencesDifferentiator(5, 0.01)
    val f = (x: Double) => x * x
    val derivative = diff.differentiate(f)

    an[IllegalArgumentException] should be thrownBy {
      derivative(1.0)(-1)
    }
  }

  it should "throw IllegalArgumentException for order >= nbPoints" in {
    val diff = new FiniteDifferencesDifferentiator(5, 0.01)
    val f = (x: Double) => x * x
    val derivative = diff.differentiate(f)

    an[IllegalArgumentException] should be thrownBy {
      derivative(1.0)(5)
    }
    an[IllegalArgumentException] should be thrownBy {
      derivative(1.0)(10)
    }
  }

  behavior of "differentiateToWrapper method"

  it should "compute function value and all derivatives up to maxOrder" in {
    val diff = new FiniteDifferencesDifferentiator(7, 0.0001)
    val f = (x: Double) => x * x * x // f'(x) = 3x^2, f''(x) = 6x, f'''(x) = 6

    val wrapper = diff.differentiateToWrapper(f, 2.0, 3)

    wrapper.getPartialDerivative(Array(0)) shouldBe 8.0 +- 1e-4 // f(2) = 8
    wrapper.getPartialDerivative(Array(1)) shouldBe 12.0 +- 1e-4 // f'(2) = 12
    wrapper.getPartialDerivative(Array(2)) shouldBe 12.0 +- 1e-3 // f''(2) = 12
    wrapper.getPartialDerivative(Array(3)) shouldBe 6.0 +- 1e-2 // f'''(2) = 6
  }

  it should "work with maxOrder = 0" in {
    val diff = new FiniteDifferencesDifferentiator(5, 0.01)
    val f = (x: Double) => x * x + 1.0

    val wrapper = diff.differentiateToWrapper(f, 3.0, 0)

    wrapper.getPartialDerivative(Array(0)) shouldBe 10.0 +- 1e-8
  }

  it should "throw IllegalArgumentException for negative maxOrder" in {
    val diff = new FiniteDifferencesDifferentiator(5, 0.01)
    val f = (x: Double) => x * x

    an[IllegalArgumentException] should be thrownBy {
      diff.differentiateToWrapper(f, 1.0, -1)
    }
  }

  it should "throw IllegalArgumentException for maxOrder >= nbPoints" in {
    val diff = new FiniteDifferencesDifferentiator(5, 0.01)
    val f = (x: Double) => x * x

    an[IllegalArgumentException] should be thrownBy {
      diff.differentiateToWrapper(f, 1.0, 5)
    }
    an[IllegalArgumentException] should be thrownBy {
      diff.differentiateToWrapper(f, 1.0, 10)
    }
  }

  behavior of "differentiator with bounded domain"

  it should "handle points near boundaries correctly" in {
    val diff = new FiniteDifferencesDifferentiator(5, 0.01, -10.0, 10.0)
    val f = (x: Double) => x * x
    val derivative = diff.differentiate(f)

    // Should work near boundaries
    noException should be thrownBy {
      derivative(0.0)(1)
      derivative(5.0)(1)
      derivative(-5.0)(1)
    }
  }

  behavior of "differentiator serialization"

  it should "be serializable" in {
    val diff = new FiniteDifferencesDifferentiator(5, 0.01, -10.0, 10.0)
    diff shouldBe a[Serializable]
  }

  behavior of "differentiator with constant function"

  it should "compute zero first derivative for constant function" in {
    val diff = new FiniteDifferencesDifferentiator(5, 0.01)
    val f = (x: Double) => 5.0
    val derivative = diff.differentiate(f)

    derivative(0.0)(1) shouldBe 0.0 +- 1e-10
    derivative(10.0)(1) shouldBe 0.0 +- 1e-10
  }

  behavior of "differentiator accuracy with different step sizes"

  it should "improve accuracy with smaller step sizes" in {
    val f = (x: Double) => math.sin(x)
    val x = 1.0
    val expected = math.cos(1.0)

    val diff1 = new FiniteDifferencesDifferentiator(5, 0.1)
    val diff2 = new FiniteDifferencesDifferentiator(5, 0.01)
    val diff3 = new FiniteDifferencesDifferentiator(5, 0.001)

    val error1 = math.abs(diff1.differentiate(f)(x)(1) - expected)
    val error2 = math.abs(diff2.differentiate(f)(x)(1) - expected)
    val error3 = math.abs(diff3.differentiate(f)(x)(1) - expected)

    // Smaller step size should give smaller error (generally)
    error2 should be < error1
    error3 should be < error2
  }
}