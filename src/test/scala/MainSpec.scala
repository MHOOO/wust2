package frontend

import org.scalatest._

class ExampleSpec extends FreeSpec with Matchers {
  println(s"######## test: productionMode = ${scala.scalajs.LinkingInfo.productionMode}")
  println(s"######## test: calling Main.main()...")
  Main.main()
}

