name := "wust"
version in ThisBuild := "0.1.0-SNAPSHOT"

enablePlugins(ScalaJSPlugin, ScalaJSBundlerPlugin)

scalaVersion in ThisBuild := "2.11.8"
libraryDependencies += "org.scalatest" %%% "scalatest" % "3.0.1" % "test"

