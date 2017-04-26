name := "wust"
version in ThisBuild := "0.1.0-SNAPSHOT"

scalaVersion in ThisBuild := "2.11.10"

enablePlugins(ScalaJSPlugin, ScalaJSBundlerPlugin) // it works without bundler

libraryDependencies += "org.scalatest" %%% "scalatest" % "3.0.1" % "test"
