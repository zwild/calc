import Dependencies._

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "io.github.zwild",
      scalaVersion := "2.12.3",
      version      := "0.1.0-SNAPSHOT"
    )),
    name := "calc.scala",
    libraryDependencies += scalaTest % Test,
    libraryDependencies += scalaParserCombinator
  )
