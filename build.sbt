val scala2Version = "2.13.6"
val scala3Version = "3.2.0"

lazy val root = project
  .in(file("."))
  .settings(
    name := "scala3-cross",
    version := "0.1.0",
    libraryDependencies ++= Seq(
      "com.github.sisyphsu" % "dateparser" % "1.0.7",
      "org.scalatest" %% "scalatest" % "3.2.9" % Test,
      "org.scalatestplus" %% "scalacheck-1-15" % "3.2.9.0" % Test,
      "com.github.sbt" % "junit-interface" % "0.13.3" % Test
    ),

    // To make the default compiler and REPL use Dotty
    scalaVersion := scala3Version,

    // To cross compile with Scala 3 and Scala 2
    crossScalaVersions := Seq(scala3Version, scala2Version)
  )
