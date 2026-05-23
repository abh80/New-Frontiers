package bench.precision

import bench.support.PrecisionResult
import java.nio.file.{Files, Path, Paths}

/** Runs all precision checks and writes benchmarks/target/precision.json. */
object PrecisionMain:
  def main(args: Array[String]): Unit =
    val out: Path = Paths.get(if args.nonEmpty then args(0) else "benchmarks/target/precision.json")
    Files.createDirectories(out.getParent)
    val results =
      VectorPrecisionChecks.run() ++
      TimePrecisionChecks.run() ++
      KinematicPrecisionChecks.run() ++
      DifferentiatorPrecisionChecks.run()
    PrecisionResult.write(out, results)
    println(s"Wrote ${results.size} precision results to $out")
