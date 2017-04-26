name := "wust"

scalaVersion in ThisBuild := "2.11.11"

lazy val root = project.in(file("."))
  .aggregate(frontend)


lazy val frontend = project
  .enablePlugins(ScalaJSPlugin, ScalaJSBundlerPlugin)
  .settings(
    libraryDependencies += "org.scalatest" %%% "scalatest" % "3.0.3" % "test",
    jsDependencies += RuntimeDOM
  )

