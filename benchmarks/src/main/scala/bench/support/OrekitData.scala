package bench.support

import org.orekit.data.{DataContext, ZipJarCrawler}

import java.nio.file.{Files, StandardCopyOption}

/** Loads the pinned orekit-data snapshot into Orekit's default DataContext once.
 *  Safe to call from every JMH @Setup; later calls do nothing. */
object OrekitData:
  @volatile private var loaded = false

  def ensureLoaded(): Unit = synchronized {
    if !loaded then
      val in = getClass.getResourceAsStream("/orekit-data.zip")
      require(in != null, "orekit-data.zip not found on classpath (benchmarks/src/main/resources)")
      val tmp = Files.createTempFile("orekit-data", ".zip")
      try Files.copy(in, tmp, StandardCopyOption.REPLACE_EXISTING)
      finally in.close()
      tmp.toFile.deleteOnExit()
      DataContext.getDefault().getDataProvidersManager().addProvider(new ZipJarCrawler(tmp.toFile))
      loaded = true
  }
