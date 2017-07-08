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
  //TODO: generic key instead of string
  def toId(t: T): String
  def toHtml(t: T, render: RenderHtml[T]): Node
  def isEqual(t1: T, t2: T): Boolean
}

trait CanBuildNestedHtml[T] extends CanBuildHtml[T] {
  def children(element: T): Seq[T]
}

case class RenderParent[T](element: T, renderHtml: RenderHtml[T])

trait RenderHtml[T] {
  def nodeMap: Map[String, Node]
  def elements: Seq[T]
  def parent: Option[RenderParent[T]]
  def update(elements: Seq[T]): Unit

  // def nextElement(id: String): Option[T] = elements.
  // def prevElement(id: String): Option[T]

  // private def findPreviousMap(trees: Seq[Tree[A]]): Map[Tree[A], Tree[A]] = {
  //   val sortedTrees = trees.sortBy(_.element)
  //   sortedTrees.drop(1).zip(sortedTrees).toMap ++ trees.map(tree => findPreviousMap(tree.children)).fold(Map.empty)(_ ++ _)
  // }
  // private def findNextMap(trees: Seq[Tree[A]]): Map[Tree[A], Tree[A]] = {
  //   val sortedTrees = trees.sortBy(_.element)
  //   sortedTrees.zip(sortedTrees.drop(1)).toMap ++ trees.map(tree => findNextMap(tree.children)).fold(Map.empty)(_ ++ _)
  // }
}

class RenderNestedHtml[T](inner: RenderHtml[T], createRenderHtml: (Node, RenderParent[T]) => RenderHtml[T])(implicit canBuild: CanBuildNestedHtml[T]) extends RenderHtml[T] {
  import canBuild._

  private var members = Map.empty[String, RenderHtml[T]]

  def nodeMap = inner.nodeMap
  def elements = inner.elements
  def parent = inner.parent

  def update(elements: Seq[T]): Unit = {
    inner.update(elements)

    members = elements.map { elem =>
      val id = toId(elem)

      val htmlNode = inner.nodeMap(id)
      val parent = RenderParent[T](elem, inner)
      val render = members.getOrElse(id, new RenderNestedHtml[T](createRenderHtml(htmlNode, parent), createRenderHtml))
      render.update(children(elem))

      id -> render
    }.toMap
  }
}

class RenderHtmlList[T](baseHtml: Node, val parent: Option[RenderParent[T]] = None, placeholder: () => Option[Node] = () => None)(implicit canBuild: CanBuildHtml[T]) extends RenderHtml[T] {
  import canBuild._

  private object existing {
    var nodeMap = Map.empty[String, Node]
    var elements = Seq.empty[T]
    var placeholder: Option[Node] = None
  }

  def nodeMap = existing.nodeMap
  def elements = existing.elements

  def update(elements: Seq[T]): Unit = {
    // remove existing elements that are missing in the new element list
    val removedIds = existing.nodeMap.keySet filterNot elements.map(toId).toSet
    for {
      id <- removedIds
      existingHtml <- existing.nodeMap.get(id)
    } baseHtml.removeChild(existingHtml)

    // show a placeholder if there are no elements, remove it if not neccessary anymore
    val newPlaceholder = if (elements.isEmpty) {
      existing.placeholder orElse placeholder().map(baseHtml.appendChild _)
    } else {
      existing.placeholder.foreach(baseHtml.removeChild _)
      None
    }

    // replace existing elements if they have changed
    val previousElements = existing.elements.collect { case element if !removedIds(toId(element)) => Some(element) }.toStream ++ Stream.continually(None)
    val newNodeMap = (elements zip previousElements).map { case (elem, prevElem) =>
      val elemHtml = prevElem match {
        case Some(prevElem) =>
          val existingHtml = existing.nodeMap(toId(prevElem))
          if (!isEqual(prevElem, elem)) {
            val newHtml = toHtml(elem, this)
            baseHtml.replaceChild(newHtml, existingHtml)
          } else existingHtml
        case None =>
          val newHtml = toHtml(elem, this)
          baseHtml.appendChild(newHtml)
      }

      toId(elem) -> elemHtml
    }.toMap

    existing.placeholder = newPlaceholder
    existing.nodeMap = newNodeMap
    existing.elements = elements
  }
}
