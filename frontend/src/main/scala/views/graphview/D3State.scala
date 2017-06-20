package wust.frontend.views.graphview

import org.scalajs.d3v4._
import org.scalajs.dom.{window,console}
import scala.scalajs.js
import scala.scalajs.js.annotation._
import scala.scalajs.js.JSConverters._
import vectory._
import Math._
import collection.mutable

@ScalaJSDefined
abstract class CustomForce[N <: SimulationNode] extends js.Object {
  def initialize(nodes: js.Array[N]): Unit = {}
  def force(alpha: Double): Unit
}
object CustomForce {
  implicit def asD3Force[N <: SimulationNode](customForce: CustomForce[N]): Force[N] = {
    val f: js.Function1[Double, Unit] = customForce.force _
    f.asInstanceOf[js.Dynamic].initialize = customForce.initialize _
    f.asInstanceOf[Force[N]]
  }
}

class RectBound {
  var xOffset: Double = -500
  var yOffset: Double = -500
  var width: Double = 1000
  var height: Double = 1000
  var strength: Double = 2

  def force(n: Int, nodes: js.Array[SimPost], pos: js.Array[Double], vel: js.Array[Double], alpha: Double) {
    val k = alpha * strength
    var i = 0
    while (i < n) {
      val node = nodes(i)
      val xRadius = node.collisionRadius //node.size.x / 2
      val yRadius = node.collisionRadius //node.size.y / 2
      val xPos = pos(2 * i) - xOffset
      val yPos = pos(2 * i + 1) - yOffset
      if (xPos < xRadius) {
        pos(2 * i) = xRadius + xOffset
        // vel(2 * i) =  vel(2 * i) + (xRadius - xPos) * k
      }
      if (yPos < yRadius) {
        pos(2 * i + 1) = yRadius + yOffset
        // vel(2 * i + 1) = vel(2 * i + 1) + (yRadius - yPos) * k
      }
      if (xPos > width - xRadius) {
        pos(2 * i) = width - xRadius + xOffset
        // vel(2 * i) = vel(2 * i) + ((width - xRadius) - xPos) * k
      }
      if (yPos > height - yRadius) {
        pos(2 * i + 1) = height - yRadius + yOffset
        // vel(2 * i + 1) = vel(2 * i + 1) + ((height - yRadius) - yPos) * k
      }
      i += 1
    }
  }
}

@ScalaJSDefined
class MetaForce extends CustomForce[SimPost] {
  var n: Int = 0
  var i: Int = 0
  var nodes = js.Array[SimPost]()
  var pos: js.Array[Double] = js.Array()
  var vel: js.Array[Double] = js.Array()
  var indices: js.Array[Int] = js.Array()
  var quadtree: Quadtree[Int] = d3.quadtree()

  override def initialize(nodes: js.Array[SimPost]) {
    this.nodes = nodes
    if (nodes.size != n) {
      n = nodes.size
      pos = new js.Array(n * 2)
      vel = new js.Array(n * 2)
      indices = (0 until n).toJSArray
    }
  }

  val rectBound = new RectBound

  def neighbours(quadtree: Quadtree[Int], x: Double, y: Double, r: Double) {
    val inCircle = mutable.ArrayBuffer.empty[Int]
    val r2 = r * r
    console.log("neighbours!")
    quadtree.visit{
      (node: QuadtreeNode[Int], x0: Double, y0: Double, x1: Double, y1: Double) =>
        console.log(node.length)
        def innerNode = node.length.isDefined
        if(innerNode) {
          
        } else {// leaf node
        }
        false
    }
  }

  override def force(alpha: Double) {
    //read pos + vel from simpost
    i = 0
    while (i < n) {
      pos(i * 2) = nodes(i).x.get
      pos(i * 2 + 1) = nodes(i).y.get
      vel(i * 2) = nodes(i).vx.get
      vel(i * 2 + 1) = nodes(i).vy.get
      i += 1
    }

    quadtree = d3.quadtree(
      indices,
      x = (i: Int) => pos(2 * i),
      y = (i: Int) => pos(2 * i + 1)
    )

    def jitter = scala.util.Random.nextDouble

    // repel
    // val mind = 600
    var ai = 0
    while (ai < n) {
      val ax = pos(ai * 2)
      val ay = pos(ai * 2 + 1)
      val a = Vec2(ax, ay)
      neighbours(quadtree, ax, ay, 500)

      // val bi = quadtree.find(ax, ay) // closest to a
      // val bx = pos(bi * 2)
      // val by = pos(bi * 2 + 1)
      // val b = Vec2(bx, by)

      // val dist = (a - b).length
      // if (dist > 0) {
      //   val bdir = (b - a) / dist
      //   val adir = bdir * -1

      //   if (dist < mind) {
      //     vel(ai * 2) += adir.x * alpha
      //     vel(ai * 2 + 1) += adir.y * alpha
      //   }

      // } else {
        // pos(ai * 2) += jitter
        // pos(ai * 2 + 1) += jitter
        // pos(bi * 2) += jitter
        // pos(bi * 2 + 1) += jitter
      // }

      ai += 1
    }

    // apply forces
    rectBound.force(n, nodes, pos, vel, alpha)
    //write pos + vel to simpost
    i = 0
    while (i < n) {
      nodes(i).x = pos(i * 2)
      nodes(i).y = pos(i * 2 + 1)
      nodes(i).vx = vel(i * 2)
      nodes(i).vy = vel(i * 2 + 1)
      i += 1
    }
    //TODO: render and simulate directly on pos and vel
  }
}

class Forces {
  // val gravityX = d3.forceX[SimPost]()
  // val gravityY = d3.forceY[SimPost]()
  val repel = d3.forceManyBody[SimPost]()
  val collision = d3.forceCollide[SimPost]() //TODO: rectangle collision detection?
  // val distance = d3.forceCollide[SimPost]()
  val connection = d3.forceLink[SimPost, SimConnection]()
  val redirectedConnection = d3.forceLink[SimPost, SimRedirectedConnection]()
  val containment = d3.forceLink[SimPost, SimContainment]()
  val collapsedContainment = d3.forceLink[SimPost, SimCollapsedContainment]()
  //TODO: push posts out of containment clusters they don't belong to
  val meta = new MetaForce

  def updatedPostSizes(posts: js.Array[SimPost]) {
    repel.initialize(posts)
    collision.initialize(posts)
  }
}

object Forces {
  def apply() = {
    val forces = new Forces

    forces.repel.strength((p: SimPost) => -p.collisionRadius * 5)
    forces.repel.distanceMax(1000)
    // forces.repel.theta(0.0) // 0 disables approximation

    forces.collision.radius((p: SimPost) => p.collisionRadius)
    // forces.collision.strength(0.9)

    // forces.distance.radius((p: SimPost) => p.collisionRadius + 600)
    // forces.distance.strength(0.01)

    forces.connection.distance(200)
    forces.connection.strength(0.3)
    forces.redirectedConnection.distance(200)
    forces.redirectedConnection.strength(0.2)

    forces.containment.distance(300)
    forces.containment.strength(0.02)
    // forces.collapsedContainment.distance(400)
    // forces.collapsedContainment.strength(0.05)

    // forces.gravityX.strength(0.001)
    // forces.gravityY.strength(0.001)

    forces
  }
}

object Simulation {
  def apply(forces: Forces): Simulation[SimPost] = d3.forceSimulation[SimPost]()
    .alphaMin(0.05) // stop simulation earlier (default = 0.001)
    // .alphaTarget(1)
    // .force("gravityx", forces.gravityX)
    // .force("gravityy", forces.gravityY)
    // .force("repel", forces.repel)
    .force("collision", forces.collision)
    // // .force("distance", forces.distance)
    .force("meta", forces.meta)
  // .force("connection", forces.connection)
  // .force("redirectedConnection", forces.redirectedConnection)
  // .force("containment", forces.containment)
  // .force("collapsedContainment", forces.collapsedContainment)
}

// TODO: run simulation in tests. jsdom timer bug?
// When running tests with d3-force in jsdom, the d3-timer does not stop itself.
// It should stop when alpha < alphaMin, but is running infinitely, causing a jsdom timeout.
class D3State(disableSimulation: Boolean = false) {
  //TODO: dynamic by screen size, refresh on window resize, put into centering force
  val zoom = d3.zoom().on("zoom.settransform", () => zoomed()).scaleExtent(js.Array(0.1, 10))
  //TODO why does this not work on 2.12, works on 2.11. maybe scalajs function implicit?
  // private def zoomed() { _transform = d3.event.asInstanceOf[ZoomEvent].transform }
  private def zoomed() = { _transform = d3.event.asInstanceOf[ZoomEvent].transform }
  private var _transform: Transform = d3.zoomIdentity // stores current pan and zoom
  def transform = _transform

  val forces = Forces()
  val simulation = Simulation(forces)
  if (disableSimulation) simulation.stop()
}
