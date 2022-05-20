import Dependencies._

name := "monad-combinable-error"
organization := "org.ornamental"
version := "0.1.0"

ThisBuild / scalaVersion := "2.13.4"

ThisBuild / scalacOptions ++= List("-deprecation" /*, "-Ymacro-debug-verbose"*/ )

lazy val macros = (project in file("macros")).settings(
  libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value,
  libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value,
  libraryDependencies ++= List(catsCore, catsEffect, shapeless),
  addCompilerPlugin(kindProjector)
)

lazy val core = (project in file("core"))
  .settings(
    libraryDependencies ++= List(catsCore, catsEffect, shapeless),
    addCompilerPlugin(kindProjector)
  )
  .dependsOn(macros)
