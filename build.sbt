ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.6"

lazy val root = (project in file("."))
  .settings(
    name := "New Frontiers",
    idePackagePrefix := Some("org.abh80.nf")
  )

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.19" % "test"