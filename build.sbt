name := "wust"
version in ThisBuild := "0.1.0-SNAPSHOT"

scalaVersion in ThisBuild := "2.11.8"

enablePlugins(ScalaJSPlugin, ScalaJSBundlerPlugin)

npmDependencies in Compile ++= (
  "d3" -> "4.6.0" ::
  Nil
)

// scalaJSOptimizerOptions in fastOptJS ~= { _.withDisableOptimizer(true) }
