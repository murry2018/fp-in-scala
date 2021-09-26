ThisBuild / organization := "com.example"
ThisBuild / scalaVersion := "3.0.1"

cancelable in Global := true

lazy val test = (project in file("."))
  .settings(
    name := "fp-in-scala",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.9" % Test
  )
