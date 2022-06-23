ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.1.3"

lazy val root = (project in file("."))
  .settings(
    name := "portrait"
  )

scalacOptions += "-Xcheck-macros"

libraryDependencies += "org.scalameta" %% "munit" % "0.7.29" % Test

testFrameworks += new TestFramework("munit.Framework")
