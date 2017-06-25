package wust.frontend.views.graphview

import org.scalajs.d3v4._
import org.scalajs.dom.{ window, console }
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

object ForceUtil {
  def forAllPointsInCircle(quadtree: Quadtree[Int], x: Double, y: Double, r: Double)(code: Int => Any): Unit = {
    quadtree.visit{
      (n: QuadtreeNode[Int], x0: Double, y0: Double, x1: Double, y1: Double) =>
        def isLeaf = !n.length.isDefined
        var node = n
        if (isLeaf) {
          do {
            code(node.data)
            node = node.next
          } while (node != js.undefined)
        }
        val rw = x1 - x0
        val rh = y1 - y0
        val rwh = rw * 0.5
        val rhh = rh * 0.5
        val centerX = x0 + rwh
        val centerY = y0 + rhh
        !Algorithms.intersectCircleAARect(x, y, r, centerX, centerY, rw, rh)
    }
  }

  def jitter = scala.util.Random.nextDouble
}

class RectBound {
  var xOffset: Double = -500
  var yOffset: Double = -500
  var width: Double = 1000
  var height: Double = 1000
  var strength: Double = 2

  def force(n: Int, nodes: js.Array[SimPost], pos: IndexedSeq[Double], vel: js.Array[Double], alpha: Double) {
    val k = alpha * strength
    var i = 0
    while (i < n) {
      val node = nodes(i)
      val xRadius = node.radius //node.size.x / 2
      val yRadius = node.radius //node.size.y / 2
      val xPos = pos(2 * i) - xOffset
      val yPos = pos(2 * i + 1) - yOffset
      if (xPos < xRadius) {
        // pos(2 * i) = xRadius + xOffset
        vel(2 * i) = vel(2 * i) + (xRadius - xPos) * k
        // vel(2 * i) =  0
      }
      if (yPos < yRadius) {
        // pos(2 * i + 1) = yRadius + yOffset
        vel(2 * i + 1) = vel(2 * i + 1) + (yRadius - yPos) * k
        // vel(2 * i + 1) = 0
      }
      if (xPos > width - xRadius) {
        // pos(2 * i) = width - xRadius + xOffset
        vel(2 * i) = vel(2 * i) + ((width - xRadius) - xPos) * k
        // vel(2 * i) = 0
      }
      if (yPos > height - yRadius) {
        // pos(2 * i + 1) = height - yRadius + yOffset
        vel(2 * i + 1) = vel(2 * i + 1) + ((height - yRadius) - yPos) * k
        // vel(2 * i + 1) = 0
      }
      i += 1
    }
  }
}

class KeepDistance {
  import ForceUtil._

  val minVisibleDistance = 150

  def force(n: Int, nodes: js.Array[SimPost], pos: IndexedSeq[Double], vel: js.Array[Double], quadtree: Quadtree[Int], maxRadius: Double, alpha: Double) {
    var ai = 0
    while (ai < n) {
      val ax = pos(ai * 2)
      val ay = pos(ai * 2 + 1)
      forAllPointsInCircle(quadtree, ax, ay, nodes(ai).radius + minVisibleDistance + maxRadius){ bi =>
        if (bi != ai) {
          val bx = pos(bi * 2)
          val by = pos(bi * 2 + 1)

          // val centerDist = (b - a).length
          val centerDist = Math.sqrt((bx - ax) * (bx - ax) + (by - ay) * (by - ay))
          // if (centerDist == 0) {
          //   pos(bi * 2) += jitter
          //   pos(bi * 2 + 1) += jitter
          // } else {
          val visibleDist = centerDist - nodes(ai).radius - nodes(bi).radius
          if (visibleDist < minVisibleDistance) {
            val dirx = (bx - ax) / centerDist
            val diry = (by - ay) / centerDist
            val strength = (1 - visibleDist / minVisibleDistance) * nodes(ai).radius
            val pushx = dirx * strength * alpha
            val pushy = diry * strength * alpha
            // println(s"dist: $visibleDist, strength: $strength, push: ${push.length}")

            vel(bi * 2) += pushx
            vel(bi * 2 + 1) += pushy
          }
          // }
        }
      }

      ai += 1
    }
  }
}

class PushOutOfWrongCluster {
  import ForceUtil._

  val minVisibleDistance = 150

  def force(n: Int, nodes: js.Array[SimPost], pos: IndexedSeq[Double], vel: js.Array[Double], quadtree: Quadtree[Int], maxRadius: Double, clusters: IndexedSeq[ContainmentCluster], alpha: Double) {
    var ci = 0
    val cn = clusters.size
    var ai = 0
    while (ci < cn) {
      val cluster = clusters(ci)
      val hull = ConvexPolygon(cluster.convexHull.map(p => Vec2(p._1, p._2)))

      ai = 0
      while (ai < n) {
        val center = Vec2(pos(2 * ai), pos(2 * ai + 1))
        val radius = nodes(ai).radius + minVisibleDistance

        val visuallyInCluster = hull intersects Circle(center, radius)
        if (visuallyInCluster) {
          val belongsToCluster = cluster.posts.exists(_.id == nodes(ai).id)
          if (!belongsToCluster) {
            val closestEdge = hull.edges.minBy(_.segmentDistance(center)) //TODO: stop earlier if under threshold
            val pointOnLine = closestEdge pointProjection center

            val dir = {
              if (closestEdge leftOf center) { // circle center outside
                (center - pointOnLine).normalized
              } else { // circle center inside
                (pointOnLine - center).normalized
              }
            }

            val strength = {
              if (closestEdge leftOf center) { // circle center outside
                (1 - (center - pointOnLine).length / radius) * nodes(ai).radius
              } else { // circle center inside
                nodes(ai).radius
              }
            }

            val pushDir = dir * strength * alpha

            // push out
            vel(ai * 2) += pushDir.x
            vel(ai * 2 + 1) += pushDir.y
          }
        }

        ai += 1
      }

      ci += 1
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

  var containmentClusters: js.Array[ContainmentCluster] = js.Array()

  override def initialize(nodes: js.Array[SimPost]) {
    this.nodes = nodes
    if (nodes.size != n) {
      n = nodes.size
      pos = new js.Array(n * 2)
      vel = new js.Array(n * 2)
      indices = (0 until n).toJSArray
    }
  }

  def setContainmentClusters(clusters: js.Array[ContainmentCluster]) {
    containmentClusters = clusters
  }

  val rectBound = new RectBound
  val keepDistance = new KeepDistance
  val pushOutOfWrongCluster = new PushOutOfWrongCluster

  override def force(alpha: Double) {
    var maxRadius = 0.0
    //read pos + vel from simpost
    i = 0
    while (i < n) {
      pos(i * 2) = nodes(i).x.get
      pos(i * 2 + 1) = nodes(i).y.get
      vel(i * 2) = nodes(i).vx.get
      vel(i * 2 + 1) = nodes(i).vy.get
      maxRadius = maxRadius max nodes(i).radius
      i += 1
    }

    quadtree = d3.quadtree(
      indices,
      x = (i: Int) => pos(2 * i),
      y = (i: Int) => pos(2 * i + 1)
    )

    // apply forces
    rectBound.force(n, nodes, pos, vel, alpha)
    keepDistance.force(n, nodes, pos, vel, quadtree, maxRadius, alpha)
    pushOutOfWrongCluster.force(n, nodes, pos, vel, quadtree, maxRadius, containmentClusters, alpha)

    //write pos + vel to simpost
    i = 0
    while (i < n) {
      // currently no force modifies the positions directly
      // nodes(i).x = pos(i * 2)
      // nodes(i).y = pos(i * 2 + 1)
      nodes(i).vx = vel(i * 2)
      nodes(i).vy = vel(i * 2 + 1)
      i += 1
    }
    //TODO: render and simulate directly on pos and vel
  }
}

class Forces {
  val gravityX = d3.forceX[SimPost]()
  val gravityY = d3.forceY[SimPost]()
  val repel = d3.forceManyBody[SimPost]()
  val collision = d3.forceCollide[SimPost]() //TODO: rectangle collision detection?
  // val distance = d3.forceCollide[SimPost]()
  val connection = d3.forceLink[SimPost, SimConnection]()
  val redirectedConnection = d3.forceLink[SimPost, SimRedirectedConnection]()
  val containment = d3.forceLink[SimPost, SimContainment]()
  val collapsedContainment = d3.forceLink[SimPost, SimCollapsedContainment]()
  //TODO: push posts out of containment clusters they don't belong to
  val meta = new MetaForce
}

object Forces {
  def apply() = {
    val forces = new Forces

    forces.repel.strength((p: SimPost) => -p.radius * 5)
    forces.repel.distanceMax(1000)
    // forces.repel.theta(0.0) // 0 disables approximation

    forces.collision.radius((p: SimPost) => p.radius)
    // forces.collision.strength(0.9)

    // forces.distance.radius((p: SimPost) => p.radius + 600)
    // forces.distance.strength(0.01)

    forces.connection.distance((c: SimConnection) => c.source.radius + 150 + c.target.radius)
    // forces.connection.strength(0.3)
    forces.redirectedConnection.distance((c: SimRedirectedConnection) => c.source.radius + 150 + c.target.radius)
    // forces.redirectedConnection.strength(0.2)

    forces.containment.distance((c: SimContainment) => c.parent.radius + 150 + c.child.radius)
    forces.containment.strength(0.2)
    // forces.collapsedContainment.distance(400)
    // forces.collapsedContainment.strength(0.05)

    forces.gravityX.strength(0.01)
    forces.gravityY.strength(0.01)

    forces
  }
}

object Simulation {
  def apply(forces: Forces): Simulation[SimPost] = d3.forceSimulation[SimPost]()
    .alphaMin(0.001) // stop simulation earlier (default = 0.001)
    // .alphaTarget(1)
    // .force("gravityx", forces.gravityX)
    // .force("gravityy", forces.gravityY)
    // .force("repel", forces.repel)
    // .force("collision", forces.collision)
    // // .force("distance", forces.distance)
    .force("meta", forces.meta)
    .force("connection", forces.connection)
    .force("redirectedConnection", forces.redirectedConnection)
    .force("containment", forces.containment)
    .force("collapsedContainment", forces.collapsedContainment)
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
