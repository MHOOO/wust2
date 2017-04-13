import org.scalatest._

class ExampleSpec extends FlatSpec with Matchers {
  println(s"######## test: productionMode = ${scala.scalajs.LinkingInfo.productionMode}")
}
