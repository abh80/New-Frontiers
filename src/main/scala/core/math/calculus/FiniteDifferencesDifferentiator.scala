package org.abh80.nf
package core.math.calculus

import scala.math._

/**
 * Implements finite differences method for numerical differentiation of functions.
 * This class uses Newton's divided differences formula to compute derivatives
 * of arbitrary order for a given function.
 *
 * @param nbPoints Number of points to use in the differentiation scheme (must be > 1)
 * @param stepSize Step size between successive points (must be > 0)
 * @param tMin     Minimum abscissa value (can be Double.NegativeInfinity)
 * @param tMax     Maximum abscissa value (can be Double.PositiveInfinity)
 * @note Even though not a `case` class, the instances of this class are guaranteed to be immutable
 */
class FiniteDifferencesDifferentiator (val nbPoints: Int, val stepSize: Double,
                                               val tMin: Double, val tMax: Double) extends Serializable {

  private val halfSampleSpan: Double = 0.5 * stepSize * (nbPoints - 1)

  if (nbPoints <= 1) {
    throw new IllegalArgumentException(s"Number of points must be > 1, got $nbPoints")
  }
  if (stepSize <= 0) {
    throw new IllegalArgumentException(s"Step size must be > 0, got $stepSize")
  }
  if (2 * halfSampleSpan >= tMax - tMin) {
    throw new IllegalArgumentException(s"Sample span ${2 * halfSampleSpan} is too large for bounds interval ${tMax - tMin}")
  }

  /**
   * Constructs a differentiator with unbounded domain.
   *
   * @param nbPoints Number of points to use in the differentiation scheme
   * @param stepSize Step size between successive points
   */
  def this(nbPoints: Int, stepSize: Double) = {
    this(nbPoints, stepSize, Double.NegativeInfinity, Double.PositiveInfinity)
  }

  /**
   * Differentiates the input function and returns a function that provides the derivative of the specified order.
   * @param f The function to differentiate.
   * @return A function that takes a point x and returns a function from order to derivative value.
   */
  def differentiate(f: Double => Double): Double => Int => Double = { x =>
    val (t0, y, a, tPoints) = computeSamplePoints(x)(f)

    order =>
      if (order < 0) {
        throw new IllegalArgumentException(s"Derivation order must be >= 0, got $order")
      }
      if (order >= nbPoints) {
        throw new IllegalArgumentException(s"Derivation order $order is too large, max is ${nbPoints - 1}")
      }
      var coeffs = buildPolynomialCoefficients(tPoints, a)
      for (_ <- 1 to order) {
        coeffs = differentiatePolynomial(coeffs)
      }
      evaluatePolynomial(coeffs, x)
  }

  /**
   * Differentiates the input function at point x and returns a DerivativeWrapper containing the function value
   * and all partial derivatives up to maxOrder.
   * @param f The function to differentiate.
   * @param x The point at which to evaluate the derivatives.
   * @param maxOrder The maximum derivation order (must be less than nbPoints).
   * @return A DerivativeWrapper containing the function value and derivatives.
   */
  def differentiateToWrapper(f: Double => Double, x: Double, maxOrder: Int): DerivativeWrapper = {
    if (maxOrder < 0) {
      throw new IllegalArgumentException(s"Max order must be >= 0, got $maxOrder")
    }
    if (maxOrder >= nbPoints) {
      throw new IllegalArgumentException(s"Max order $maxOrder is too large, max is ${nbPoints - 1}")
    }

    // Compute sample points and function values


    val (t0, y, a, tPoints) = computeSamplePoints(x)(f)

    // Compute all derivatives up to maxOrder
    val data = new Array[Double](maxOrder + 1)
    var coeffs = buildPolynomialCoefficients(tPoints, a)
    for (order <- 0 to maxOrder) {
      data(order) = evaluatePolynomial(coeffs, x)
      if (order < maxOrder) {
        coeffs = differentiatePolynomial(coeffs)
      }
    }

    new DerivativeWrapper(1, maxOrder, data)
  }

  private def computeSamplePoints(x: Double)(implicit f: Double => Double): (Double, Array[Double], Array[Double], Array[Double]) = {
    val t0 = max(min(x, tMax), tMin) - halfSampleSpan
    val y = Array.tabulate(nbPoints)(i => f(t0 + i * stepSize))
    val a = computeNewtonCoefficients(y)
    val tPoints = Array.tabulate(nbPoints)(i => t0 + i * stepSize)
    (t0, y, a, tPoints)
  }

  /**
   * Computes the coefficients in Newton's divided differences formula.
   *
   * @param y Array of function values at sample points
   * @return Array of Newton coefficients
   */
  private def computeNewtonCoefficients(y: Array[Double]): Array[Double] = {
    val bottom = y.clone()
    val a = new Array[Double](nbPoints)
    for (i <- 0 until nbPoints) {
      for (j <- 1 to i) {
        bottom(i - j) = (bottom(i - j + 1) - bottom(i - j)) / (j * stepSize)
      }
      a(i) = bottom(0)
    }
    a
  }

  /**
   * Builds the coefficients of the interpolation polynomial.
   *
   * @param tPoints Array of sample points
   * @param a       Array of Newton coefficients
   * @return Array of polynomial coefficients in ascending degree order
   */
  private def buildPolynomialCoefficients(tPoints: Array[Double], a: Array[Double]): Array[Double] = {
    val poly = new Array[Double](nbPoints)
    val prod = new Array[Double](nbPoints)
    prod(0) = 1.0
    var currentDegree = 0
    for (i <- 0 until nbPoints) {
      val ai = a(i)
      for (j <- 0 to currentDegree) {
        poly(j) += ai * prod(j)
      }
      if (i < nbPoints - 1) {
        val newProd = new Array[Double](nbPoints)
        val tj = tPoints(i)
        for (j <- 0 to currentDegree) {
          newProd(j) -= tj * prod(j)
        }
        for (j <- 1 to currentDegree + 1) {
          newProd(j) += prod(j - 1)
        }
        System.arraycopy(newProd, 0, prod, 0, nbPoints)
        currentDegree += 1
      }
    }
    poly
  }

  /**
   * Computes the coefficients of the derivative of a polynomial.
   *
   * @param coeffs Array of polynomial coefficients in ascending degree order
   * @return Array of coefficients of the derivative polynomial
   */
  private def differentiatePolynomial(coeffs: Array[Double]): Array[Double] = {
    val degree = coeffs.length - 1
    if (degree < 1) {
      Array(0.0)
    } else {
      val newCoeffs = new Array[Double](degree)
      for (i <- 0 until degree) {
        newCoeffs(i) = (i + 1) * coeffs(i + 1)
      }
      newCoeffs
    }
  }

  /**
   * Evaluates a polynomial at a given point using Horner's method.
   *
   * @param coeffs Array of polynomial coefficients in ascending degree order
   * @param x      Point at which to evaluate the polynomial
   * @return Value of the polynomial at x
   */
  private def evaluatePolynomial(coeffs: Array[Double], x: Double): Double = {
    if (coeffs.isEmpty) 0.0 else {
      var sum = coeffs.last
      for (i <- coeffs.length - 2 to 0 by -1) {
        sum = sum * x + coeffs(i)
      }
      sum
    }
  }
}