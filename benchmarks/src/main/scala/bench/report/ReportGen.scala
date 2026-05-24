package bench.report

import bench.support.PrecisionResult

import java.nio.file.{Files, Paths}

object ReportGen:
  final case class TimingRow(category: String, operation: String, library: String, nsPerOp: Double, bytesPerOp: Double)

  /** Parse JMH's results.json into timing rows. Method names follow `<lib>_<op>`,
   *  class names follow `<Category>Benchmarks`. */
  def parseJmh(json: String): Seq[TimingRow] =
    ujson.read(json).arr.toSeq.map { o =>
      val fqcn = o("benchmark").str                 // e.g. bench.vector.Vector3DBenchmarks.nf_add
      val method = fqcn.substring(fqcn.lastIndexOf('.') + 1)
      val className = fqcn.substring(0, fqcn.lastIndexOf('.'))
      val simpleClass = className.substring(className.lastIndexOf('.') + 1)
      val category = simpleClass.stripSuffix("Benchmarks")
      val sep = method.indexOf('_')
      val library = method.substring(0, sep)
      val operation = method.substring(sep + 1)
      val pm = o("primaryMetric")
      val ns = pm("score").num
      // JMH puts `secondaryMetrics` at the benchmark level, sibling of `primaryMetric`.
      val bytes =
        o.obj.get("secondaryMetrics").flatMap { sm =>
          sm.obj.find(_._1.endsWith("gc.alloc.rate.norm")).map(_._2("score").num)
        }.getOrElse(Double.NaN)
      TimingRow(category, operation, library, ns, bytes)
    }

  /** Compact number for a Mermaid axis: integers stay integers, tiny values collapse to 0. */
  private def num(d: Double): String =
    if d.isNaN || math.abs(d) < 1e-3 then "0"
    else
      val r = BigDecimal(d).round(new java.math.MathContext(4)).toDouble
      if r == r.toLong.toDouble then r.toLong.toString else r.toString

  /** Turn an absolute error into "accurate decimal digits" = -log10(error), capped at 18
   *  (attosecond-ish). 0 error is exact, so it gets the cap. Higher is better. */
  private def digits(e: Double): Double =
    if e <= 0.0 then 18.0 else math.max(0.0, math.min(18.0, -math.log10(e)))

  private def lineChart(title: String, yTitle: String, ops: Seq[String], nf: Seq[Double], ok: Seq[Double]): String =
    val xs  = ops.map(o => "\"" + o + "\"").mkString(", ")
    val nfs = nf.map(num).mkString(", ")
    val oks = ok.map(num).mkString(", ")
    s"""```mermaid
xychart-beta
    title "$title"
    x-axis [$xs]
    y-axis "$yTitle"
    line [$nfs]
    line [$oks]
```
*First line: New Frontiers. Second line: Orekit/Hipparchus.*
"""

  /** A README with Mermaid line charts per category: speed and memory together, error separately. */
  def buildMarkdown(timing: Seq[TimingRow], precision: Seq[PrecisionResult]): String =
    val sb = new StringBuilder
    sb.append("# New Frontiers vs Orekit/Hipparchus benchmark baseline\n\n")
    sb.append("Lower is better for speed and memory; higher is better for precision. Every chart has two\n")
    sb.append("lines, New Frontiers first and Orekit/Hipparchus second. Speed (ns/op) and memory (bytes/op)\n")
    sb.append("come from JMH. Precision is shown as accurate decimal digits, `-log10(absolute error)`, capped\n")
    sb.append("at 18; a point at 18 means the result was exact. Regenerate with `sbt benchAll`.\n\n")

    def cell(rows: Seq[TimingRow], cat: String, op: String, lib: String, f: TimingRow => Double): Double =
      rows.find(r => r.category == cat && r.operation == op && r.library == lib).map(f).getOrElse(Double.NaN)

    val cats = (timing.map(_.category) ++ precision.map(_.category)).distinct.sorted
    cats.foreach { cat =>
      sb.append(s"## $cat\n\n")

      val tOps = timing.filter(_.category == cat).map(_.operation).distinct.sorted
      if tOps.nonEmpty then
        sb.append("### Performance and memory\n\n")
        sb.append(lineChart(s"$cat speed (ns/op, lower is better)", "ns/op", tOps,
          tOps.map(o => cell(timing, cat, o, "nf", _.nsPerOp)),
          tOps.map(o => cell(timing, cat, o, "ok", _.nsPerOp))))
        sb.append("\n")
        sb.append(lineChart(s"$cat memory (bytes/op, lower is better)", "bytes/op", tOps,
          tOps.map(o => cell(timing, cat, o, "nf", _.bytesPerOp)),
          tOps.map(o => cell(timing, cat, o, "ok", _.bytesPerOp))))
        sb.append("\n")

      val pOps = precision.filter(_.category == cat).map(_.operation).distinct.sorted
      if pOps.nonEmpty then
        def err(op: String, lib: String): Double =
          precision.find(r => r.category == cat && r.operation == op && r.library == lib).map(_.error).getOrElse(Double.NaN)
        sb.append("### Precision\n\n")
        sb.append(lineChart(s"$cat precision (accurate digits, higher is better)", "digits", pOps,
          pOps.map(o => digits(err(o, "nf"))),
          pOps.map(o => digits(err(o, "ok")))))
        sb.append("\n")
    }
    sb.toString

  def main(args: Array[String]): Unit =
    val jmhPath = Paths.get(if args.length > 0 then args(0) else "benchmarks/target/results.json")
    val prcPath = Paths.get(if args.length > 1 then args(1) else "benchmarks/target/precision.json")
    val outDir  = Paths.get(if args.length > 2 then args(2) else "benchmarks/result")
    Files.createDirectories(outDir)

    val timing = parseJmh(Files.readString(jmhPath))
    val precision = PrecisionResult.read(prcPath)

    val out = outDir.resolve("README.md")
    Files.writeString(out, buildMarkdown(timing, precision))
    println(s"Wrote $out (${timing.size} timing rows, ${precision.size} precision rows)")
