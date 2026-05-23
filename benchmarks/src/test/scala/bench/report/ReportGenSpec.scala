package bench.report

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ReportGenSpec extends AnyFlatSpec with Matchers:
  // Mirrors real JMH JSON: `secondaryMetrics` is a sibling of `primaryMetric`, not nested in it.
  private val jmhJson =
    """[
      | {"benchmark":"bench.vector.Vector3DBenchmarks.nf_add",
      |  "primaryMetric":{"score":3.0,"scoreUnit":"ns/op"},
      |  "secondaryMetrics":{"gc.alloc.rate.norm":{"score":24.0,"scoreUnit":"B/op"}}},
      | {"benchmark":"bench.vector.Vector3DBenchmarks.ok_add",
      |  "primaryMetric":{"score":6.0,"scoreUnit":"ns/op"},
      |  "secondaryMetrics":{"gc.alloc.rate.norm":{"score":48.0,"scoreUnit":"B/op"}}}
      |]""".stripMargin

  "ReportGen.parseJmh" should "extract category, op, library, ns/op and B/op" in {
    val rows = ReportGen.parseJmh(jmhJson)
    rows should contain (ReportGen.TimingRow("Vector3D", "add", "nf", 3.0, 24.0))
    rows should contain (ReportGen.TimingRow("Vector3D", "add", "ok", 6.0, 48.0))
  }

  it should "find the gc alloc metric even when JMH prefixes the key with a middot" in {
    val withDot = jmhJson.replace("\"gc.alloc.rate.norm\"", "\"·gc.alloc.rate.norm\"")
    ReportGen.parseJmh(withDot).head.bytesPerOp shouldBe 24.0
  }
