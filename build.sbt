ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.0"

lazy val root = (project in file("."))
  .settings(
    name := "VipingWhiley"
  )

libraryDependencies += "org.typelevel" %% "cats-parse" % "0.3.9"
libraryDependencies += "org.scalatest" %% "scalatest-funsuite" % "3.2.17" % "test"
