ThisBuild / scalaVersion     := "2.13.12"
ThisBuild / version          := "0.1.0"
ThisBuild / organization     := "Intelligent Digital Systems Lab"

val chiselVersion = "6.2.0"

// Common settings for all projects
lazy val commonSettings = Seq(
  scalacOptions ++= Seq(
    "-deprecation",
    "-feature",
  ),
  javaOptions += "-Xmx2g",
  Test / fork := true,
  run / fork := true
)

// Root project
lazy val root = (project in file("."))
  .settings(
    name := "aigis",
    libraryDependencies ++= Seq(
      "org.chipsalliance" %% "chisel" % chiselVersion,
      "org.scalatest" %% "scalatest" % "3.2.16" % "test"
    ),
    scalacOptions ++= Seq(
      "-language:reflectiveCalls",
      "-Xcheckinit",
      "-Ymacro-annotations",
    ),
    addCompilerPlugin("org.chipsalliance" % "chisel-plugin" % chiselVersion cross CrossVersion.full),
    Test / test := {
      throw new MessageOnlyException(
        """
          |ERROR: Direct test execution is disabled.
          |Please use Makefile
          |""".stripMargin
      )
    },
    Test / testOnly := {
      throw new MessageOnlyException(
        """
          |ERROR: Direct test execution is disabled.
          |Please use Makefile
          |""".stripMargin
      )
    }
  )