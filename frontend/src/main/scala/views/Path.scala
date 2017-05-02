package wust.frontend.views

import org.scalajs.dom._
import rx._
import wust.ids._
import wust.graph._

import scala.collection.breakOut
import scala.util.{Failure, Success, Try}
import scalatags.JsDom.TypedTag
import scalatags.JsDom.all._
import scalatags.rx.all._

case class Path(name: String, options: Map[String,String]) {
  override def toString: String = {
    if (options.isEmpty) name
    else name + "?" + mapToQuery(options)
  }

  private def mapToQuery(query: Map[String, Any]): String =
    query.map { case (k, v) => s"$k=$v" }.mkString("&")
}
object Path {
  private val pathRe = "([^?]+)(?:\\?([^=]+=[^&]+(?:&[^=]+=[^&]+)*))?".r

  def unapply(str: String): Option[Path] = str match {
    case pathRe(path, query) =>
      val map = Option(query).map(queryToMap).getOrElse(Map.empty)
      Option(Path(path, map))
    case _ => None
  }

  private def queryToMap(query: String): Map[String, String] =
    query.split("&").map { parts =>
      val Array(key, value) = parts.split("=")
      key -> value
    }.toMap
}

object PathOption {
  object IdList {
    def parse(str: String): Seq[Long] = str.split(",").flatMap(part => Try(part.toLong).toOption)
    def toString(seq: Seq[Long]): String = seq.mkString(",")
  }

  object Flag {
    def parse(str: String): Boolean = Try(str.toBoolean).toOption.getOrElse(false)
    def toString(flag: Boolean): String = flag.toString
  }
}