package wust.frontend

import scala.scalajs.LinkingInfo
object DevOnly {
  def apply[T](code: => T): Option[T] = {
    if (LinkingInfo.developmentMode)
      Some(code)
    else
      None
  }
}

object DevPrintln {
  def apply(code: => String) {
    DevOnly { println(code) }
  }
}
