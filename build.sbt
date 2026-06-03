ThisBuild / scalaVersion := "3.3.6"
ThisBuild / crossScalaVersions := Seq("3.3.6", "2.13.14")

ThisBuild / organization := "io.github.abh80"
ThisBuild / organizationName := "abh80"
ThisBuild / description := "High-precision astrodynamics and orbital-mechanics library, cross-built for Scala 2.13 and Scala 3."
ThisBuild / homepage := Some(url("https://github.com/abh80/new-frontiers"))
ThisBuild / licenses := Seq(
  "Nokia" -> url("https://spdx.org/licenses/Nokia.html")
)
ThisBuild / developers := List(
  Developer(
    id = "abh80",
    name = "abh80",
    email = "abh80@users.noreply.github.com",
    url = url("https://github.com/abh80")
  )
)
ThisBuild / scmInfo := Some(
  ScmInfo(
    url("https://github.com/abh80/new-frontiers"),
    "scm:git:https://github.com/abh80/new-frontiers.git",
    "scm:git:git@github.com:abh80/new-frontiers.git"
  )
)
ThisBuild / versionScheme := Some("early-semver")

// Version is the single source of truth for releases — pinned HERE, not derived from the
// git tag. sbt-ci-release still drives signing + Sonatype Central publishing, but this
// explicit setting overrides sbt-dynver's git-derived version. The Release workflow refuses
// to publish unless the pushed `vX.Y.Z` tag matches this string. Bump this to cut a release.
ThisBuild / version := "0.1.0"

// Publish to the Sonatype Central Portal (central.sonatype.com), not the legacy OSSRH host.
ThisBuild / sonatypeCredentialHost := xerial.sbt.Sonatype.sonatypeCentralHost

// Add per-Scala-version source directories (src/main/scala-3, src/main/scala-2.13) on top of
// the shared src/main/scala tree. Sources in `scala-3/` only compile on Scala 3; `scala-2.13/`
// only on Scala 2.13. The Scala-3-only constructs (enum, trait params) live in scala-3/ with
// equivalent sealed-trait + case-object mirrors in scala-2.13/.
def perVersionScalaSrc(scalaVer: String, base: File): Seq[File] =
  CrossVersion.partialVersion(scalaVer) match {
    case Some((3, _)) => Seq(base / "scala-3")
    case Some((2, _)) => Seq(base / "scala-2.13")
    case _            => Nil
  }

lazy val root = (project in file("."))
  .settings(
    name := "new-frontiers",
    idePackagePrefix := Some("org.abh80.nf"),
    Compile / unmanagedSourceDirectories ++=
      perVersionScalaSrc(scalaVersion.value, (Compile / sourceDirectory).value),
    Test / unmanagedSourceDirectories ++=
      perVersionScalaSrc(scalaVersion.value, (Test / sourceDirectory).value),
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.19" % Test
  )

// Java-idiomatic facade. Wraps the Scala API with named methods (cross / dot / approxEquals
// instead of operators), java.util.Optional in place of Option, and factory helpers for
// classes whose Scala-only constructors use implicit params. Published as `new-frontiers-java`
// alongside the cross-built core. Scala 3 only — Java consumers ignore the `_3` suffix and just
// add the Scala 3 stdlib.
lazy val javaFacade = (project in file("java-facade"))
  .dependsOn(root)
  .settings(
    name := "new-frontiers-java",
    crossScalaVersions := Seq("3.3.6"),
    scalaVersion := "3.3.6",
    libraryDependencies ++= Seq(
      "org.scalatest"     %% "scalatest"        % "3.2.19" % Test,
      "junit"             %  "junit"            % "4.13.2" % Test,
      "com.github.sbt"    %  "junit-interface"  % "0.13.3" % Test
    ),
    Test / testOptions += Tests.Argument(TestFrameworks.JUnit, "-v")
  )

// Benchmark-only module. Compares New Frontiers against Orekit/Hipparchus.
// Never published: keeps Orekit/JMH/XChart off the library artifact.
lazy val benchmarks = (project in file("benchmarks"))
  .dependsOn(root)
  .enablePlugins(JmhPlugin)
  .settings(
    name := "new-frontiers-benchmarks",
    publish / skip := true,
    crossScalaVersions := Seq("3.3.6"),
    scalaVersion := "3.3.6",
    libraryDependencies ++= Seq(
      "org.orekit"      %  "orekit"             % "12.2",
      "org.hipparchus"  %  "hipparchus-core"    % "3.1",
      "org.hipparchus"  %  "hipparchus-geometry"% "3.1",
      "org.knowm.xchart"%  "xchart"             % "3.8.8",
      "com.lihaoyi"     %% "ujson"              % "3.3.1",
      "org.scalatest"   %% "scalatest"          % "3.2.19" % Test
    )
  )

// Full pipeline: timing+memory (JMH) -> precision -> report+charts.
addCommandAlias(
  "benchAll",
  ";benchmarks/Jmh/run -prof gc -rf json -rff target/results.json " +
    ";benchmarks/runMain bench.precision.PrecisionMain " +
    ";benchmarks/runMain bench.report.ReportGen"
)
