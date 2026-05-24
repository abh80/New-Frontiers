package bench.report

import bench.support.PrecisionResult

import org.knowm.xchart.{CategoryChartBuilder, BitmapEncoder}
import org.knowm.xchart.BitmapEncoder.BitmapFormat
import org.knowm.xchart.CategorySeries.CategorySeriesRenderStyle
import org.knowm.xchart.style.Styler.LegendPosition

import java.awt.Color
import java.nio.file.{Files, Path, Paths}
import scala.jdk.CollectionConverters.*

object ReportGen:
  final case class TimingRow(category: String, operation: String, library: String, nsPerOp: Double, bytesPerOp: Double)

  private val NF_COLOR    = new Color(0x25, 0x63, 0xeb) // blue
  private val OK_COLOR    = new Color(0xf5, 0x9e, 0x0b) // amber
  private val ERROR_FLOOR = 1e-18 // floor so the table's accuracy ratio stays finite
  private val BYTES_FLOOR = 1.0   // floor so the table's memory ratio stays finite

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

  private def safeRatio(ok: Double, nf: Double): Double =
    val r = ok / nf
    if r.isNaN || r.isInfinite || r <= 0 then 1.0 else r

  /** Compact number: integers stay integers, tiny/huge go scientific. */
  private def fmt(d: Double): String =
    if d.isNaN then "n/a"
    else if d == 0.0 then "0"
    else if math.abs(d) < 1e-3 || math.abs(d) >= 1e6 then f"$d%.2e"
    else if d == d.toLong.toDouble then d.toLong.toString
    else f"$d%.3f"

  private def fmtRatio(r: Double): String =
    if r >= 100 then f"$r%.0f×" else f"$r%.2f×"

  /** Grouped bar chart: New Frontiers vs Orekit/Hipparchus per op, actual values, lower is better.
   *  `fileBase` is the path without `.png`. */
  private def writeGroupedChart(fileBase: Path, title: String, yTitle: String,
                                ops: Seq[String], nf: Seq[Double], ok: Seq[Double]): Unit =
    val chart = new CategoryChartBuilder()
      .width(860).height(380).title(title).xAxisTitle("operation").yAxisTitle(yTitle).build()
    val st = chart.getStyler
    st.setLegendPosition(LegendPosition.InsideNW)
    st.setXAxisLabelRotation(20)
    st.setPlotGridVerticalLinesVisible(false)

    def toJava(xs: Seq[Double]) =
      xs.map(d => java.lang.Double.valueOf(if d.isNaN then 0.0 else d)).asJava

    val nfSeries = chart.addSeries("New Frontiers", ops.asJava, toJava(nf))
    nfSeries.setFillColor(NF_COLOR); nfSeries.setLineColor(NF_COLOR)
    val okSeries = chart.addSeries("Orekit/Hipparchus", ops.asJava, toJava(ok))
    okSeries.setFillColor(OK_COLOR); okSeries.setLineColor(OK_COLOR)

    BitmapEncoder.saveBitmap(chart, fileBase.toString, BitmapFormat.PNG)

  /** README embedding the ratio charts, with exact numbers underneath for reference. */
  def buildMarkdown(timing: Seq[TimingRow], precision: Seq[PrecisionResult]): String =
    val sb = new StringBuilder
    sb.append("# New Frontiers vs Orekit/Hipparchus benchmark baseline\n\n")
    sb.append("Each chart compares New Frontiers (blue) and Orekit/Hipparchus (amber) per operation;\n")
    sb.append("lower bars are better. Speed (ns/op) and memory (bytes/op) come from JMH. Precision is left\n")
    sb.append("as a table, since errors span many orders of magnitude and an exact result has zero error,\n")
    sb.append("which does not plot well. The tables also give the plain NF-to-Orekit ratio. Regenerate with `sbt benchAll`.\n\n")

    val cats = (timing.map(_.category) ++ precision.map(_.category)).distinct.sorted
    cats.foreach { cat =>
      sb.append(s"## $cat\n\n")

      val tOps = timing.filter(_.category == cat).map(_.operation).distinct.sorted
      if tOps.nonEmpty then
        def t(op: String, lib: String, f: TimingRow => Double): Double =
          timing.find(r => r.category == cat && r.operation == op && r.library == lib).map(f).getOrElse(Double.NaN)
        sb.append("### Performance and memory\n\n")
        sb.append(s"![$cat speed](./$cat-speed.png)\n\n")
        sb.append(s"![$cat memory](./$cat-memory.png)\n\n")
        sb.append("| op | NF ns/op | Orekit ns/op | speed | NF B/op | Orekit B/op | memory |\n")
        sb.append("|----|---------:|-------------:|------:|--------:|------------:|-------:|\n")
        tOps.foreach { op =>
          val nfNs = t(op, "nf", _.nsPerOp); val okNs = t(op, "ok", _.nsPerOp)
          val nfB  = t(op, "nf", _.bytesPerOp); val okB = t(op, "ok", _.bytesPerOp)
          sb.append(s"| $op | ${fmt(nfNs)} | ${fmt(okNs)} | ${fmtRatio(safeRatio(okNs, nfNs))} " +
            s"| ${fmt(nfB)} | ${fmt(okB)} | ${fmtRatio(safeRatio(math.max(okB, BYTES_FLOOR), math.max(nfB, BYTES_FLOOR)))} |\n")
        }
        sb.append("\n")

      val pOps = precision.filter(_.category == cat).map(_.operation).distinct.sorted
      if pOps.nonEmpty then
        def e(op: String, lib: String): Double =
          precision.find(r => r.category == cat && r.operation == op && r.library == lib).map(_.error).getOrElse(Double.NaN)
        sb.append("### Precision\n\n")
        sb.append("| op | NF error | Orekit error | accuracy |\n")
        sb.append("|----|---------:|-------------:|---------:|\n")
        pOps.foreach { op =>
          val nfE = e(op, "nf"); val okE = e(op, "ok")
          val acc = safeRatio(math.max(okE, ERROR_FLOOR), math.max(nfE, ERROR_FLOOR))
          sb.append(s"| $op | ${if nfE == 0 then "0 (exact)" else fmt(nfE)} | ${if okE == 0 then "0 (exact)" else fmt(okE)} | ${fmtRatio(acc)} |\n")
        }
        sb.append("\n")
    }
    sb.toString

  /** Render every ratio PNG referenced by the README. */
  private def writeCharts(outDir: Path, timing: Seq[TimingRow], precision: Seq[PrecisionResult]): Unit =
    val cats = (timing.map(_.category) ++ precision.map(_.category)).distinct.sorted
    cats.foreach { cat =>
      val tOps = timing.filter(_.category == cat).map(_.operation).distinct.sorted
      if tOps.nonEmpty then
        def t(op: String, lib: String, f: TimingRow => Double): Double =
          timing.find(r => r.category == cat && r.operation == op && r.library == lib).map(f).getOrElse(Double.NaN)
        writeGroupedChart(outDir.resolve(s"$cat-speed"), s"$cat speed (ns/op, lower is better)", "ns/op", tOps,
          tOps.map(t(_, "nf", _.nsPerOp)), tOps.map(t(_, "ok", _.nsPerOp)))
        writeGroupedChart(outDir.resolve(s"$cat-memory"), s"$cat memory (bytes/op, lower is better)", "bytes/op", tOps,
          tOps.map(t(_, "nf", _.bytesPerOp)), tOps.map(t(_, "ok", _.bytesPerOp)))
      // Precision is reported as a table only (no chart).
    }

  def main(args: Array[String]): Unit =
    val jmhPath = Paths.get(if args.length > 0 then args(0) else "benchmarks/target/results.json")
    val prcPath = Paths.get(if args.length > 1 then args(1) else "benchmarks/target/precision.json")
    val outDir  = Paths.get(if args.length > 2 then args(2) else "benchmarks/result")
    Files.createDirectories(outDir)

    val timing = parseJmh(Files.readString(jmhPath))
    val precision = PrecisionResult.read(prcPath)

    writeCharts(outDir, timing, precision)
    Files.writeString(outDir.resolve("README.md"), buildMarkdown(timing, precision))
    println(s"Wrote ${outDir.resolve("README.md")} + ratio charts (${timing.size} timing rows, ${precision.size} precision rows)")
