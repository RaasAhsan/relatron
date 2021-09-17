import sbt._

object Dependencies {
  object MUnit {
    val version = "0.7.29"
    val core = "org.scalameta" %% "munit" % version
    val scalacheck = "org.scalameta" %% "munit-scalacheck" % version
  }
}
