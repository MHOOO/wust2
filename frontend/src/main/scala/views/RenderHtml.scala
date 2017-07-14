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
import scala.util.Try
import scalatags.rx.all._
import scala.scalajs.js.timers.setTimeout
import org.scalajs.dom.ext.KeyCode
import org.scalajs.dom.{ Event, KeyboardEvent }

trait CanBuildHtml[T] {
  def toId(t: T): String
  def toHtml(t: T, render: RenderHtml[T]): Node
  def isEqual(t1: T, t2: T): Boolean
}

trait CanBuildNestedHtml[T] extends CanBuildHtml[T] {
  def children(element: T): Seq[T]
}

case class RenderParent[T](element: T, renderHtml: RenderHtml[T])

trait RenderHtml[T] {
  //TODO: generic key instead of string
  def getHtmlNode(id: String): Option[Node]
  def elements: Seq[T]
  def parent: Option[RenderParent[T]]
  def update(elements: Seq[T]): Unit
}

class RenderNestedHtml[T](inner: RenderHtml[T], createRenderHtml: (Node, RenderParent[T]) => RenderHtml[T])(implicit canBuild: CanBuildNestedHtml[T]) extends RenderHtml[T] {
  import canBuild._

  private var members = Map.empty[String, RenderHtml[T]]

  def getHtmlNode(id: String) = inner.getHtmlNode(id)
  def elements = inner.elements
  def parent = inner.parent

  def update(elements: Seq[T]): Unit = {
    // get the dom nodes before the update
    val previousNodeMap = elements.flatMap { elem =>
      val id = toId(elem)
      inner.getHtmlNode(id).map(id -> _)
    }.toMap

    // update inner html render
    inner.update(elements)

    // for all new elements:
    // - if element has children, create html render for children in element's dom node:
    //    - if dom node in the inner html render has changed: create new html render for this element within the new dom node
    //    - else: reuse existing html render
    //    - update render for children of this element
    members = elements.flatMap { elem =>
      val id = toId(elem)
      val htmlNode = inner.getHtmlNode(id).get

      def newHtmlRender() = {
        println("render: new html render " + id)
        val innerBaseHtml = div().render
        htmlNode.appendChild(innerBaseHtml)
        new RenderNestedHtml[T](createRenderHtml(innerBaseHtml, RenderParent[T](elem, inner)), createRenderHtml)
      }

      val childElements = children(elem)
      if (childElements.isEmpty) {
        members.get(id).foreach(_ => htmlNode.removeChild(htmlNode.lastChild))
        None
      } else {
        val render = members.get(id) match {
          case Some(prevRender) =>
            val prevHtmlNode = previousNodeMap.get(id)
            val sameNode = prevHtmlNode.fold(false)(_ isSameNode htmlNode)
            if (sameNode) prevRender
            else newHtmlRender()
          case None => newHtmlRender()
        }

        render.update(childElements)

        Some(id -> render)
      }
    }.toMap
  }
}

class RenderHtmlList[T](baseHtml: Node, val parent: Option[RenderParent[T]] = None, placeholder: () => Option[Node] = () => None)(implicit canBuild: CanBuildHtml[T]) extends RenderHtml[T] {
  import canBuild._

  private object existing {
    var indexMap = Map.empty[String, Int]
    var elements = Seq.empty[T]
    var placeholder: Option[Node] = None
  }

  def getHtmlNode(id: String) = existing.indexMap.get(id).map(baseHtml.childNodes(_))
  def elements = existing.elements

  def update(elements: Seq[T]): Unit = {
    val newElementIds = elements.map(toId).toSet
    val (remainingElements, removedElements) = existing.elements.partition(elem => newElementIds(toId(elem)))

    // remove existing elements that are missing in the new element list
    removedElements.map { elem =>
      val id = toId(elem)
      val index = existing.indexMap(id)
      println("render: removing element " + id)
      baseHtml.childNodes(index)
    }.foreach(baseHtml.removeChild _)

    // show a placeholder if there are no elements, remove it if not neccessary anymore
    val newPlaceholder = if (elements.isEmpty) {
      existing.placeholder orElse placeholder().map(baseHtml.appendChild _)
    } else {
      existing.placeholder.foreach(baseHtml.removeChild _)
      None
    }

    // for all new elements:
    // - if there already exists an element at this index:
    //    - if element at position has changed: replace the dom node
    //    - else: keep existing node
    // - else: append new node to html node
    val newIndexMap = (elements zip remainingElements.toStream.map(Some(_)) ++ Stream.continually(None)).zipWithIndex.map { case ((elem, prevElem), index) =>
      val id = toId(elem)
      prevElem match {
        case Some(prevElem) =>
          if (!isEqual(prevElem, elem)) {
            println("render: replacing element " + id)
            val newHtml = toHtml(elem, this)
            val currHtml = baseHtml.childNodes(index)
            Try(baseHtml.replaceChild(newHtml, currHtml)).failed.foreach { _ =>
              console.error("render wtf")
            }
          }
        case None =>
          println("render: new element " + id)
          val newHtml = toHtml(elem, this)
          baseHtml.appendChild(newHtml)
      }

      id -> index
    }.toMap

    //set new state
    existing.placeholder = newPlaceholder
    existing.indexMap = newIndexMap
    existing.elements = elements
  }
}
