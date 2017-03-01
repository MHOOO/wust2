package frontend

import scala.scalajs.js
import scala.scalajs.js.annotation._

// https://github.com/d3/d3-color
@JSImport("d3-color", JSImport.Namespace)
@js.native
object d3color extends js.Object {
  def lab(l:Double, a:Double, b:Double):js.Object = js.native
}

import scala.scalajs.js.JSApp

object Main extends JSApp {
  def main(): Unit = {
    val set = Set(2, 3)
    println(set.toList.map(c => d3color.lab(1,2,c)))
    println(set.map(c => d3color.lab(1,2,c)))
  }
}
