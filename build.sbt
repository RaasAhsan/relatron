import Dependencies._

ThisBuild / scalaVersion     := "3.0.2"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "com.example"
ThisBuild / organizationName := "example"

lazy val root = (project in file("."))
  .settings(
    name := "relprog",
    Compile / sourceGenerators += Def.task {
      val file = (Compile / sourceManaged).value / "boilerplate" / "FreshBoilerplate.scala"
      IO.write(file, Boilerplate.fresh)
      Seq(file)
    }
  )
