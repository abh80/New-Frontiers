package bench.support

import java.nio.file.{Files, Path}

/** One precision measurement: absolute error of `library`'s result for
 *  `operation` in `category`, versus the independent truth oracle. */
final case class PrecisionResult(category: String, operation: String, library: String, error: Double)

object PrecisionResult:
  def write(path: Path, results: Seq[PrecisionResult]): Unit =
    val arr = ujson.Arr(
      results.map(r =>
        ujson.Obj(
          "category"  -> r.category,
          "operation" -> r.operation,
          "library"   -> r.library,
          "error"     -> r.error
        )
      )*
    )
    Files.writeString(path, ujson.write(arr, indent = 2))

  def read(path: Path): Seq[PrecisionResult] =
    ujson.read(Files.readString(path)).arr.toSeq.map { o =>
      PrecisionResult(o("category").str, o("operation").str, o("library").str, o("error").num)
    }
