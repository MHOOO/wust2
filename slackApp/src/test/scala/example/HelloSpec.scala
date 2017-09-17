package example

import org.scalatest._

class HelloSpec extends FreeSpec with MustMatchers {
  "The Hello object" in {
    "hello" mustEqual "hello"
  }
}
