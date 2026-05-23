addSbtPlugin("org.jetbrains.scala" % "sbt-ide-settings" % "1.1.2")

// Tag-driven release to Maven Central: pulls in sbt-dynver, sbt-pgp, and sbt-sonatype.
// Provides the `ci-release` task used by the release workflow.
addSbtPlugin("com.github.sbt" % "sbt-ci-release" % "1.9.3")

// JMH micro-benchmark harness for the `benchmarks` subproject.
addSbtPlugin("pl.project13.scala" % "sbt-jmh" % "0.4.7")
