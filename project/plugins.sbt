addSbtPlugin("org.scala-js" % "sbt-scalajs" % "0.6.14")
addSbtPlugin("ch.epfl.scala" % "sbt-scalajs-bundler" % "0.5.0")
addSbtPlugin("ch.epfl.scala" % "sbt-web-scalajs-bundler" % "0.5.0")
addSbtPlugin("com.vmunier" % "sbt-web-scalajs" % "1.0.3")
addSbtPlugin("com.typesafe.sbt" % "sbt-gzip" % "1.0.0")
addSbtPlugin("com.typesafe.sbt" % "sbt-digest" % "1.1.1")
addSbtPlugin("org.neolin.sbt" % "sbt-simple-url-update" % "1.0.1") //TODO publish-local, 1.0.1 is not yet released, but fixes this bug for us: https://github.com/neomaclin/sbt-simple-url-update/issues/6

addSbtPlugin("io.spray" % "sbt-revolver" % "0.8.0")
addSbtPlugin("com.lihaoyi" % "workbench" % "0.3.0")

addSbtPlugin("se.marcuslonnberg" % "sbt-docker" % "1.4.0")
addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "0.14.3")
