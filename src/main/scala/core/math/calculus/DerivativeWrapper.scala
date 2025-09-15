package org.abh80.nf
package core.math.calculus

/**
 * A simple implementation of the Derivative trait for multivariate automatic differentiation.
 *
 * @param parameters The number of free parameters (independent variables).
 * @param maxOrder The maximum derivation order.
 * @param data An array containing the function value and all partial derivatives.
 */
class DerivativeWrapper(parameters: Int, maxOrder: Int, data: Array[Double]) extends Derivative {
  private val sizes: Array[Array[Int]] = computeSizes(parameters, maxOrder)

  /**
   * Computes the sizes array for indexing derivatives.
   * sizes[p][o] represents the number of partial derivatives for p parameters up to order o.
   */
  private def computeSizes(params: Int, order: Int): Array[Array[Int]] = {
    val sizesArr = Array.ofDim[Int](params + 1, order + 1)
    (0 to order).foreach { j => sizesArr(0)(j) = 1 } // Base case: 0 parameters
    for (p <- 1 to params) {
      sizesArr(p)(0) = 1
      for (i <- 0 until order) {
        sizesArr(p)(i + 1) = sizesArr(p)(i) + sizesArr(p - 1)(i + 1)
      }
    }
    sizesArr
  }

  /** @inheritdoc */
  override def getPartialDerivative(orders: Array[Int]): Double = {
    if (orders.length != parameters) {
      throw new IllegalArgumentException(
        s"Orders length ${orders.length} does not match parameters $parameters"
      )
    }
    val index = computeIndex(orders)
    data(index)
  }

  /**
   * Computes the index in the data array for the given orders.
   * Iterates from the last parameter to the first, accumulating skips based on sizes.
   */
  private def computeIndex(orders: Array[Int]): Int = {
    var index = 0
    var m = maxOrder
    var ordersSum = 0
    var i = parameters - 1
    while (i >= 0) {
      val derivativeOrder = orders(i)
      ordersSum += derivativeOrder
      if (ordersSum > maxOrder) {
        throw new IllegalArgumentException(s"Sum of orders $ordersSum exceeds max order $maxOrder")
      }
      var j = derivativeOrder
      while (j > 0) {
        index += sizes(i)(m)
        m -= 1
        j -= 1
        if (m < 0) {
          throw new IllegalArgumentException("Invalid derivation order")
        }
      }
      i -= 1
    }
    index
  }

  /** @inheritdoc */
  override def getFreeParameters: Int = parameters

  /** @inheritdoc */
  override def getOrder: Int = maxOrder

  // Utility to get the total size of the data array
  def getSize: Int = sizes(parameters)(maxOrder)

  override def getValue: Double = data(0)
}