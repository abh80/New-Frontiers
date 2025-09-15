package org.abh80.nf
package core.math.calculus

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class DerivativeWrapperSpec extends AnyFunSuite with Matchers {

  test("constructor with basic parameters") {
    val dw = new DerivativeWrapper(1, 1, Array(5.0, 2.0))
    dw.getValue shouldBe 5.0
    dw.getFreeParameters shouldBe 1
    dw.getOrder shouldBe 1
    dw.getSize shouldBe 2
  }

  test("constructor with zero parameters") {
    val dw = new DerivativeWrapper(0, 5, Array(3.0))
    dw.getValue shouldBe 3.0
    dw.getFreeParameters shouldBe 0
    dw.getOrder shouldBe 5
    dw.getSize shouldBe 1
  }

  test("getPartialDerivative for value (all orders zero)") {
    val dw = new DerivativeWrapper(2, 2, Array.tabulate(6)(i => i.toDouble))
    dw.getPartialDerivative(Array(0, 0)) shouldBe 0.0
  }

  test("getPartialDerivative for univariate cases") {
    val dw = new DerivativeWrapper(1, 2, Array(0.0, 1.0, 2.0))
    dw.getPartialDerivative(Array(0)) shouldBe 0.0
    dw.getPartialDerivative(Array(1)) shouldBe 1.0
    dw.getPartialDerivative(Array(2)) shouldBe 2.0
  }

  test("getPartialDerivative for bivariate order 1") {
    val dw = new DerivativeWrapper(2, 1, Array(0.0, 1.0, 2.0))
    dw.getPartialDerivative(Array(0, 0)) shouldBe 0.0
    dw.getPartialDerivative(Array(1, 0)) shouldBe 1.0
    dw.getPartialDerivative(Array(0, 1)) shouldBe 2.0
  }

  test("getPartialDerivative for bivariate order 2") {
    val dw = new DerivativeWrapper(2, 2, Array(0.0, 1.0, 2.0, 3.0, 4.0, 5.0))
    dw.getPartialDerivative(Array(0, 0)) shouldBe 0.0
    dw.getPartialDerivative(Array(1, 0)) shouldBe 1.0
    dw.getPartialDerivative(Array(2, 0)) shouldBe 2.0
    dw.getPartialDerivative(Array(0, 1)) shouldBe 3.0
    dw.getPartialDerivative(Array(1, 1)) shouldBe 4.0
    dw.getPartialDerivative(Array(0, 2)) shouldBe 5.0
  }

  test("getPartialDerivative with zero parameters") {
    val dw = new DerivativeWrapper(0, 0, Array(42.0))
    dw.getPartialDerivative(Array.empty[Int]) shouldBe 42.0
  }

  test("exception for orders array too short") {
    val dw = new DerivativeWrapper(2, 1, Array.tabulate(3)(i => i.toDouble))
    assertThrows[IllegalArgumentException] {
      dw.getPartialDerivative(Array(0))
    }
  }

  test("exception for orders array too long") {
    val dw = new DerivativeWrapper(2, 1, Array.tabulate(3)(i => i.toDouble))
    assertThrows[IllegalArgumentException] {
      dw.getPartialDerivative(Array(0, 0, 0))
    }
  }

  test("exception for orders sum exceeding max order") {
    val dw = new DerivativeWrapper(2, 1, Array.tabulate(3)(i => i.toDouble))
    assertThrows[IllegalArgumentException] {
      dw.getPartialDerivative(Array(1, 1))
    }
  }

  test("exception for individual order exceeding max order") {
    val dw = new DerivativeWrapper(1, 1, Array(0.0, 1.0))
    assertThrows[IllegalArgumentException] {
      dw.getPartialDerivative(Array(2))
    }
  }

  private def buildWrapper(param: Int = 2, maxOrder: Int = 1, data: Array[Double] = new Array(3)): DerivativeWrapper =
    new DerivativeWrapper(param, maxOrder, data)
}