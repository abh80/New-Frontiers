package bench.support

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.Files

class PrecisionResultSpec extends AnyFlatSpec with Matchers:
  "PrecisionResult" should "round-trip through JSON" in {
    val results = Seq(
      PrecisionResult("Vector3D", "normalize", "nf", 1.0e-16),
      PrecisionResult("Vector3D", "normalize", "ok", 2.0e-16)
    )
    val tmp = Files.createTempFile("precision", ".json")
    PrecisionResult.write(tmp, results)
    PrecisionResult.read(tmp) shouldBe results
  }
