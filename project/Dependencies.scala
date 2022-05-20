import sbt._

object Dependencies {

  val kindProjector =
    "org.typelevel" %% "kind-projector" % "0.11.3" cross CrossVersion.full

  val shapeless = "com.chuusai" %% "shapeless" % "2.3.7"

  val catsCore = "org.typelevel" %% "cats-core" % "2.7.0"

  val catsEffect = "org.typelevel" %% "cats-effect" % "3.3.5"
}
