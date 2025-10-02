package org.abh80.nf.core.math.calculus;

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class FiniteDifferencesDifferentiatorSpec extends AnyFunSuite with Matchers {

    test("create a differentiator with valid parameters") {
        val diff = new FiniteDifferencesDifferentiator(5, 0.01)
        diff.nbPoints shouldBe 5
        diff.stepSize shouldBe 0.01
        diff.tMin shouldBe Double.NegativeInfinity
        diff.tMax shouldBe Double.PositiveInfinity
    }

    test("create a differentiator with bounded domain") {
        val diff = new FiniteDifferencesDifferentiator(5, 0.01, -10.0, 10.0)
        diff.nbPoints shouldBe 5
        diff.stepSize shouldBe 0.01
        diff.tMin shouldBe -10.0
        diff.tMax shouldBe 10.0
    }

    test("throw IllegalArgumentException when nbPoints <= 1") {
        assertThrows[IllegalArgumentException] {
            new FiniteDifferencesDifferentiator(1, 0.01)
        }
    }

    test("throw IllegalArgumentException when stepSize <= 0") {
        assertThrows[IllegalArgumentException] {
            new FiniteDifferencesDifferentiator(5, 0.0)
        }
        assertThrows[IllegalArgumentException] {
            new FiniteDifferencesDifferentiator(5, -0.01)
        }
    }

    test("throw IllegalArgumentException when sample span is too large for bounds") {
        assertThrows[IllegalArgumentException] {
            new FiniteDifferencesDifferentiator(10, 1.0, 0.0, 5.0)
        }
    }

    test("compute first derivative of linear function correctly") {
        val diff = new FiniteDifferencesDifferentiator(3, 0.01)
        val f = (x: Double) => 2.0 * x + 3.0 // f'(x) = 2
        val derivative = diff.differentiate(f)

        derivative(0.0)(1) shouldBe 2.0 +- 1e-10
        derivative(5.0)(1) shouldBe 2.0 +- 1e-10
        derivative(-3.0)(1) shouldBe 2.0 +- 1e-10
    }

    test("compute first derivative of quadratic function correctly") {
        val diff = new FiniteDifferencesDifferentiator(5, 0.001)
        val f = (x: Double) => x * x // f'(x) = 2x
        val derivative = diff.differentiate(f)

        derivative(1.0)(1) shouldBe 2.0 +- 1e-6
        derivative(3.0)(1) shouldBe 6.0 +- 1e-6
        derivative(-2.0)(1) shouldBe -4.0 +- 1e-6
    }

    test("compute second derivative of quadratic function correctly") {
        val diff = new FiniteDifferencesDifferentiator(5, 0.001)
        val f = (x: Double) => x * x // f''(x) = 2
        val derivative = diff.differentiate(f)

        derivative(0.0)(2) shouldBe 2.0 +- 1e-5
        derivative(5.0)(2) shouldBe 2.0 +- 1e-5
    }

    test("compute derivatives of cubic function correctly") {
        val diff = new FiniteDifferencesDifferentiator(7, 0.0001)
        val f = (x: Double) => x * x * x // f'(x) = 3x^2, f''(x) = 6x, f'''(x) = 6
        val derivative = diff.differentiate(f)

        derivative(2.0)(1) shouldBe 12.0 +- 1e-4 // 3 * 2^2 = 12
        derivative(2.0)(2) shouldBe 12.0 +- 1e-3 // 6 * 2 = 12
        derivative(2.0)(3) shouldBe 6.0 +- 1e-2 // 6
    }

    test("compute zeroth derivative (function value) correctly") {
        val diff = new FiniteDifferencesDifferentiator(5, 0.01)
        val f = (x: Double) => x * x + 2.0 * x + 1.0
        val derivative = diff.differentiate(f)

        derivative(3.0)(0) shouldBe 16.0 +- 1e-8
        derivative(0.0)(0) shouldBe 1.0 +- 1e-8
    }

    test("compute derivative of sin(x) correctly") {
        val diff = new FiniteDifferencesDifferentiator(7, 0.0001)
        val f = (x: Double) => math.sin(x) // f'(x) = cos(x)
        val derivative = diff.differentiate(f)

        derivative(0.0)(1) shouldBe math.cos(0.0) +- 1e-6
        derivative(math.Pi / 4)(1) shouldBe math.cos(math.Pi / 4) +- 1e-6
        derivative(math.Pi / 2)(1) shouldBe math.cos(math.Pi / 2) +- 1e-6
    }

    test("compute derivative of exp(x) correctly") {
        val diff = new FiniteDifferencesDifferentiator(5, 0.001)
        val f = (x: Double) => math.exp(x) // f'(x) = exp(x)
        val derivative = diff.differentiate(f)

        derivative(0.0)(1) shouldBe math.exp(0.0) +- 1e-5
        derivative(1.0)(1) shouldBe math.exp(1.0) +- 1e-4
        derivative(2.0)(1) shouldBe math.exp(2.0) +- 1e-3
    }

    test("throw IllegalArgumentException for negative order") {
        val diff = new FiniteDifferencesDifferentiator(5, 0.01)
        val f = (x: Double) => x * x
        val derivative = diff.differentiate(f)

        assertThrows[IllegalArgumentException] {
            derivative(1.0)(-1)
        }
    }

    test("throw IllegalArgumentException for order >= nbPoints") {
        val diff = new FiniteDifferencesDifferentiator(5, 0.01)
        val f = (x: Double) => x * x
        val derivative = diff.differentiate(f)

        assertThrows[IllegalArgumentException] {
            derivative(1.0)(5)
        }
        assertThrows[IllegalArgumentException] {
            derivative(1.0)(10)
        }
    }

    test("compute function value and all derivatives up to maxOrder") {
        val diff = new FiniteDifferencesDifferentiator(7, 0.0001)
        val f = (x: Double) => x * x * x // f'(x) = 3x^2, f''(x) = 6x, f'''(x) = 6

        val wrapper = diff.differentiateToWrapper(f, 2.0, 3)

        wrapper.getPartialDerivative(0) shouldBe 8.0 +- 1e-6 // f(2) = 8
        wrapper.getPartialDerivative(1) shouldBe 12.0 +- 1e-4 // f'(2) = 12
        wrapper.getPartialDerivative(2) shouldBe 12.0 +- 1e-3 // f''(2) = 12
        wrapper.getPartialDerivative(3) shouldBe 6.0 +- 1e-2 // f'''(2) = 6
    }

    test("work with maxOrder = 0") {
        val diff = new FiniteDifferencesDifferentiator(5, 0.01)
        val f = (x: Double) => x * x + 1.0

        val wrapper = diff.differentiateToWrapper(f, 3.0, 0)

        wrapper.getPartialDerivative(0) shouldBe 10.0 +- 1e-8
    }

    test("throw IllegalArgumentException for negative maxOrder") {
        val diff = new FiniteDifferencesDifferentiator(5, 0.01)
        val f = (x: Double) => x * x

        assertThrows[IllegalArgumentException] {
            diff.differentiateToWrapper(f, 1.0, -1)
        }
    }

    test("throw IllegalArgumentException for maxOrder >= nbPoints") {
        val diff = new FiniteDifferencesDifferentiator(5, 0.01)
        val f = (x: Double) => x * x

        assertThrows[IllegalArgumentException] {
            diff.differentiateToWrapper(f, 1.0, 5)
        }
        assertThrows[IllegalArgumentException] {
            diff.differentiateToWrapper(f, 1.0, 10)
        }
    }

    test("handle points near boundaries correctly") {
        val diff = new FiniteDifferencesDifferentiator(5, 0.01, -10.0, 10.0)
        val f = (x: Double) => x * x
        val derivative = diff.differentiate(f)

        noException should be thrownBy {
            derivative(0.0)(1)
            derivative(5.0)(1)
            derivative(-5.0)(1)
        }
    }

    test("be serializable") {
        val diff = new FiniteDifferencesDifferentiator(5, 0.01, -10.0, 10.0)
        diff shouldBe a[Serializable]
    }

    test("compute zero first derivative for constant function") {
        val diff = new FiniteDifferencesDifferentiator(5, 0.01)
        val f = (x: Double) => 5.0
        val derivative = diff.differentiate(f)

        derivative(0.0)(1) shouldBe 0.0 +- 1e-10
        derivative(10.0)(1) shouldBe 0.0 +- 1e-10
    }

    test("improve accuracy with smaller step sizes") {
        val f = (x: Double) => math.sin(x)
        val x = 1.0
        val expected = math.cos(1.0)

        val diff1 = new FiniteDifferencesDifferentiator(5, 0.1)
        val diff2 = new FiniteDifferencesDifferentiator(5, 0.01)
        val diff3 = new FiniteDifferencesDifferentiator(5, 0.001)

        val error1 = math.abs(diff1.differentiate(f)(x)(1) - expected)
        val error2 = math.abs(diff2.differentiate(f)(x)(1) - expected)
        val error3 = math.abs(diff3.differentiate(f)(x)(1) - expected)

        error2 should be < error1
        error3 should be < error2
    }
}