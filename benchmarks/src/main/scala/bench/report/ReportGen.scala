package bench.report

import bench.support.PrecisionResult
import org.knowm.xchart.{CategoryChart, CategoryChartBuilder, BitmapEncoder}
import org.knowm.xchart.BitmapEncoder.BitmapFormat
import org.knowm.xchart.style.Styler.LegendPosition

import java.nio.file.{Files, Path, Paths}
import scala.jdk.CollectionConverters.*

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

  private def fmt(d: Double): String =
    if d.isNaN then "n/a"
    else if d == 0.0 then "0"
    else if math.abs(d) < 1e-3 || math.abs(d) >= 1e6 then f"$d%.3e"
    else f"$d%.3f"

  /** Build the markdown report. Speed/memory and precision are separate tables per category
   *  because their operation sets differ (precision uses analytic identities, not the timed ops). */
  def buildMarkdown(timing: Seq[TimingRow], precision: Seq[PrecisionResult]): String =
    val sb = new StringBuilder
    sb.append("# New Frontiers vs Orekit/Hipparchus benchmark baseline\n\n")
    sb.append("`nf` is New Frontiers, `ok` is Orekit/Hipparchus. Lower is better in every column.\n")
    sb.append("ns/op comes from JMH AverageTime, B/op from the JMH GC profiler (`gc.alloc.rate.norm`), ")
    sb.append("and error is the absolute distance from an analytic or known value. `speed x` is ok ns/op divided by nf ns/op, so above 1 means NF is faster.\n\n")

    val tByCat = timing.groupBy(_.category)
    val pByCat = precision.groupBy(_.category)
    val cats = (timing.map(_.category) ++ precision.map(_.category)).distinct.sorted

    cats.foreach { cat =>
      sb.append(s"## $cat\n\n")

      val tRows = tByCat.getOrElse(cat, Nil)
      if tRows.nonEmpty then
        sb.append("### Speed and memory\n\n")
        sb.append("| op | nf ns/op | ok ns/op | speed x | nf B/op | ok B/op |\n")
        sb.append("|----|---------:|---------:|--------:|--------:|--------:|\n")
        tRows.map(_.operation).distinct.sorted.foreach { op =>
          val nf = tRows.find(r => r.operation == op && r.library == "nf")
          val ok = tRows.find(r => r.operation == op && r.library == "ok")
          val speedup = for { n <- nf; o <- ok if n.nsPerOp != 0 } yield o.nsPerOp / n.nsPerOp
          sb.append(s"| $op | ${nf.map(r => fmt(r.nsPerOp)).getOrElse("-")} | ${ok.map(r => fmt(r.nsPerOp)).getOrElse("-")} " +
            s"| ${speedup.map(s => f"$s%.2f").getOrElse("-")} | ${nf.map(r => fmt(r.bytesPerOp)).getOrElse("-")} " +
            s"| ${ok.map(r => fmt(r.bytesPerOp)).getOrElse("-")} |\n")
        }
        sb.append(s"\n![${cat} ns/op](./${cat}-nsop.png)\n\n")

      val pRows = pByCat.getOrElse(cat, Nil)
      if pRows.nonEmpty then
        sb.append("### Precision (absolute error from an analytic or known value)\n\n")
        sb.append("| op | nf err | ok err |\n")
        sb.append("|----|-------:|-------:|\n")
        pRows.map(_.operation).distinct.sorted.foreach { op =>
          val nfErr = pRows.find(r => r.operation == op && r.library == "nf").map(_.error)
          val okErr = pRows.find(r => r.operation == op && r.library == "ok").map(_.error)
          sb.append(s"| $op | ${nfErr.map(fmt).getOrElse("-")} | ${okErr.map(fmt).getOrElse("-")} |\n")
        }
        sb.append("\n")
    }
    sb.toString

  /** One grouped bar chart per category for ns/op (NF vs OK). */
  private def writeChart(dir: Path, category: String, timing: Seq[TimingRow]): Unit =
    val ops = timing.filter(_.category == category).map(_.operation).distinct.sorted
    def series(lib: String): java.util.List[java.lang.Double] =
      ops.map(op => java.lang.Double.valueOf(
        timing.find(r => r.category == category && r.operation == op && r.library == lib).map(_.nsPerOp).getOrElse(0.0)
      )).asJava
    val chart: CategoryChart = new CategoryChartBuilder()
      .width(900).height(500).title(s"$category ns/op (lower is better)")
      .xAxisTitle("operation").yAxisTitle("ns/op").build()
    chart.getStyler.setLegendPosition(LegendPosition.InsideNW)
    chart.addSeries("New Frontiers", ops.asJava, series("nf"))
    chart.addSeries("Orekit/Hipparchus", ops.asJava, series("ok"))
    BitmapEncoder.saveBitmap(chart, dir.resolve(s"$category-nsop").toString, BitmapFormat.PNG)

  def main(args: Array[String]): Unit =
    val jmhPath = Paths.get(if args.length > 0 then args(0) else "benchmarks/target/results.json")
    val prcPath = Paths.get(if args.length > 1 then args(1) else "benchmarks/target/precision.json")
    val outDir  = Paths.get(if args.length > 2 then args(2) else "benchmarks/result")
    Files.createDirectories(outDir)

    val timing = parseJmh(Files.readString(jmhPath))
    val precision = PrecisionResult.read(prcPath)

    timing.map(_.category).distinct.foreach(cat => writeChart(outDir, cat, timing))
    Files.writeString(outDir.resolve("README.md"), buildMarkdown(timing, precision))
    println(s"Wrote report + ${timing.map(_.category).distinct.size} charts to $outDir")
