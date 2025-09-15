package org.abh80.nf
package core.math.calculus

/**
 * A trait for representing a function and its partial derivatives in a multivariate automatic differentiation system.
 * Implementations store the function value and all partial derivatives up to a specified order for a given number of free parameters.
 */
trait Derivative {
  /**
   * Retrieves the value of the partial derivative for the specified orders.
   *
   * @param orders An array specifying the order of differentiation for each free parameter.
   *               The length must match the number of free parameters, and the sum of orders must not exceed the maximum derivation order.
   * @return The value of the partial derivative corresponding to the specified orders.
   * @throws IllegalArgumentException if the orders array length does not match the number of free parameters
   *         or if the sum of orders exceeds the maximum derivation order.
   */
  def getPartialDerivative(orders: Array[Int]): Double

  /**
   * Returns the number of free parameters in the function.
   *
   * @return The number of independent variables.
   */
  def getFreeParameters: Int

  /**
   * Returns the maximum derivation order supported by this instance.
   *
   * @return The maximum order of partial derivatives.
   */
  def getOrder: Int
  
  /** Get the value part of the wrapper */
  def getValue: Double
}