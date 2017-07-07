package wust.frontend.views

import org.scalajs.d3v4
import rx._
import rxext._
import wust.frontend._
import wust.ids._
import wust.graph._
import wust.util.Pipe
import wust.util.algorithm.{ TreeContext, Tree, redundantSpanningTree }
import wust.util.collection._
import autowire._
import boopickle.Default._
import wust.api._
import scala.concurrent.ExecutionContext.Implicits.global
import scalaz.Tag
import scala.math.Ordering

import org.scalajs.dom.{ window, document, console }
import org.scalajs.dom.raw.{ Text, Element, HTMLElement, Node }
import scalatags.JsDom.all._
import scalatags.JsDom.TypedTag
import scala.scalajs.js
import scalatags.rx.all._
import scala.scalajs.js.timers.setTimeout
import org.scalajs.dom.ext.KeyCode
import org.scalajs.dom.{ Event, KeyboardEvent }

trait CanBuildHtml[T] {
  def toId(t: T): String
  def toHtml(t: T): TypedTag[HTMLElement]
  def isEqual(t1: T, t2: T): Boolean
}

trait CanBuildNestedHtml[T] extends CanBuildHtml[T] {
  def children(element: T): Seq[T]
}

trait RenderHtml[T] {
  def getHtmlNode(element: T): Option[Node]
  def update(elements: Seq[T]): Unit
}

class RenderNestedHtml[T](inner: RenderHtml[T], createRenderHtml: Node => RenderHtml[T])(implicit canBuild: CanBuildNestedHtml[T]) extends RenderHtml[T] {
  import canBuild._

  private var members = Map.empty[String, RenderHtml[T]]

  def getHtmlNode(element: T): Option[Node] = inner.getHtmlNode(element)

  def update(elements: Seq[T]): Unit = {
    inner.update(elements)

    members = elements.map { elem =>
      val id = toId(elem)

      val htmlNode = inner.getHtmlNode(elem).get // must be there!
      val render = members.getOrElse(id, new RenderNestedHtml[T](createRenderHtml(htmlNode), createRenderHtml))
      render.update(children(elem))

      id -> render
    }.toMap
  }
}

class RenderHtmlList[T](baseHtml: Node, placeholder: Option[TypedTag[HTMLElement]] = None)(implicit canBuild: CanBuildHtml[T]) extends RenderHtml[T] {
  import canBuild._

  //TODO: store dom node with T in map instead of document.getElementById?
  private var members = Map.empty[String, T]
  private var firstTime = true //TODO

  private val baseId = wust.frontend.Cuid()
  private val placeholderId = baseId + "placeholder"

  def getHtmlNode(element: T): Option[Node] = {
    val id = toId(element)
    if (members.contains(id)) {
      val htmlId = baseId + id
      Option(document.getElementById(htmlId))
    } else None
  }

  def update(elements: Seq[T]): Unit = {
    val newMembers = elements.by(toId)
    val removedIds = members.keySet filterNot newMembers.keySet
    removedIds.foreach { id =>
      val htmlId = baseId + id
      val existingHtml = document.getElementById(htmlId)
      baseHtml.removeChild(existingHtml)
    }

    //TODO sorting changed, replace

    if (elements.isEmpty) {
      placeholder.foreach { placeholder =>
        if (firstTime || members.nonEmpty) {
          val placeholderNode = placeholder(attr("id") := placeholderId).render
          baseHtml.appendChild(placeholderNode)
        }
      }
    } else {
      placeholder.foreach { placeholder =>
        if (!firstTime && members.isEmpty) {
          val placeholderNode = document.getElementById(placeholderId)
          baseHtml.removeChild(placeholderNode)
        }
      }

      elements.foreach { elem =>
        val id = toId(elem)
        val htmlId = baseId + id
        members.get(id) match {
          case Some(prevElem) =>
            if (!isEqual(prevElem, elem)) {
              val existingHtml = document.getElementById(htmlId)
              val newHtml = toHtml(elem)(attr("id") := htmlId).render
              baseHtml.replaceChild(newHtml, existingHtml)
            }
          case None =>
            val newHtml = toHtml(elem)(attr("id") := htmlId).render
            baseHtml.appendChild(newHtml)
        }
      }
    }

    firstTime = false
    members = newMembers
  }
}
