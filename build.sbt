ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.7.0"

lazy val root = (project in file("."))
  .settings(
    name := "Vara",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.19" % "test"
  )
