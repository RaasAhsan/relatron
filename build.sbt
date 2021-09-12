import Dependencies._

ThisBuild / scalaVersion     := "3.0.2"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "com.example"
ThisBuild / organizationName := "example"

lazy val root = (project in file("."))
  .settings(
    name := "relprog",
    Compile / sourceGenerators += Def.task {
      val fresh = (Compile / sourceManaged).value / "boilerplate" / "FreshBoilerplate.scala"
      val run = (Compile / sourceManaged).value / "boilerplate" / "RunBoilerplate.scala"
      IO.write(fresh, Boilerplate.fresh)
      IO.write(run, Boilerplate.run)
      Seq(fresh, run)
    }
  )
