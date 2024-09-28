ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.10"

lazy val root = (project in file("."))
  .settings(
    name := "wordbrainsolver",
    idePackagePrefix := Some("com.wordbrainsolver.application")
  )

libraryDependencies += "org.specs2" % "specs2-junit_2.13" % "4.19.2"
libraryDependencies += "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.4"
