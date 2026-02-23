val scala3Version = "3.5.2"

lazy val root = project
  .in(file("."))
  .settings(
    name         := "sfc-abm-core",
    version      := "0.2.0",
    scalaVersion := scala3Version,
    Compile / mainClass := Some("sfc.sfcMonteCarlo"),
    assembly / mainClass := Some("sfc.sfcMonteCarlo"),
    assembly / assemblyJarName := "sfc-abm.jar",
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "3.2.19" % Test
    )
  )
