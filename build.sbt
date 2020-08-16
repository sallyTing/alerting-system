import Dependencies._

ThisBuild / scalaVersion     := "2.13.2"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "com.example"
ThisBuild / organizationName := "example"

val circeVersion = "0.12.3"

lazy val root = (project in file("."))
  .settings(
    name := "Alerting System",
    libraryDependencies ++= Seq (
      "org.typelevel" %% "cats-core" % "2.0.0",
      "org.typelevel" %% "cats-effect" % "2.1.3",
      "io.circe" %% "circe-core" % circeVersion,
      "io.circe" %% "circe-generic" % circeVersion,
      "io.circe" %% "circe-parser" % circeVersion,
      scalaTest % Test
    )
  )

// See https://www.scala-sbt.org/1.x/docs/Using-Sonatype.html for instructions on how to publish to Sonatype.
