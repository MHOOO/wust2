package frontend

import scala.scalajs.js.JSApp

object Main extends JSApp {
  def main(): Unit = {
    println(s"######## run: productionMode = ${scala.scalajs.LinkingInfo.productionMode}")
  }
}
