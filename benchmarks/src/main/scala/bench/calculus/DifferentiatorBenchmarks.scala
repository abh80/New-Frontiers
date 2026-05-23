package bench.calculus

import org.openjdk.jmh.annotations.*
import java.util.concurrent.TimeUnit

import org.abh80.nf.core.math.calculus.FiniteDifferencesDifferentiator
import org.hipparchus.analysis.UnivariateFunction
import org.hipparchus.analysis.differentiation.{DSFactory, FiniteDifferencesDifferentiator as HFDD}

@State(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@Fork(1)
@Warmup(iterations = 5, time = 1)
@Measurement(iterations = 5, time = 1)
class DifferentiatorBenchmarks:
  val nbPoints = 5
  val step = 1e-3
  val x = 0.7
  val f: Double => Double = (t: Double) => math.sin(t)
  val uf: UnivariateFunction = (t: Double) => math.sin(t)
  var dsf: DSFactory = null

  @Setup(Level.Trial)
  def setup(): Unit =
    dsf = new DSFactory(1, 1)

  @Benchmark def nf_firstDerivative: Double =
    new FiniteDifferencesDifferentiator(nbPoints, step).differentiate(f)(x)(1)

  @Benchmark def ok_firstDerivative: Double =
    new HFDD(nbPoints, step).differentiate(uf).value(dsf.variable(0, x)).getPartialDerivative(1)
