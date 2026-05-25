ThisBuild / scalaVersion := "3.3.6"

ThisBuild / organization := "io.github.abh80"
ThisBuild / organizationName := "abh80"
ThisBuild / description := "High-precision astrodynamics and orbital-mechanics library for Scala 3."
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

lazy val root = (project in file("."))
  .settings(
    name := "New Frontiers",
    idePackagePrefix := Some("org.abh80.nf"),
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.19" % "test"
  )

// Benchmark-only module. Compares New Frontiers against Orekit/Hipparchus.
// Never published: keeps Orekit/JMH/XChart off the library artifact.
lazy val benchmarks = (project in file("benchmarks"))
  .dependsOn(root)
  .enablePlugins(JmhPlugin)
  .settings(
    name := "new-frontiers-benchmarks",
    publish / skip := true,
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
