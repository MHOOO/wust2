package wust.frontend.views.graphview

import org.scalajs.d3v4._
import wust.frontend.Color._

import scala.collection.breakOut
import scala.scalajs.js
import Math._
import vectory._

class ContainmentCluster(val parent: SimPost, val children: IndexedSeq[SimPost], val depth: Int) {
  val id = parent.id
  val posts: IndexedSeq[SimPost] = children :+ parent
  val postCount = posts.size
  val sn = 16
  val step = PI * 2.0 / sn
  val positionSamples = new js.Array[js.Tuple2[Double, Double]](sn * posts.size)
  def regenerateCircleSamples() {
    var i = 0
    val n = postCount * sn
    while (i < n) {
      val a = (i % sn) * step
      val post = posts(i / sn)
      val radius = post.radius + 15
      positionSamples(i) = js.Tuple2(
        cos(a) * radius + post.x.get,
        sin(a) * radius + post.y.get
      )
      i += 1
    }
  }

  def positions: js.Array[js.Tuple2[Double, Double]] = { regenerateCircleSamples(); positionSamples }
  def convexHull: js.Array[js.Tuple2[Double, Double]] = {
    val hull = d3.polygonHull(positions)
    //TODO: how to correctly handle scalajs union type?
    if (hull == null) positions
    else hull.asInstanceOf[js.Array[js.Tuple2[Double, Double]]]
  }
}

object ContainmentHullSelection extends DataSelection[ContainmentCluster] {
  override val tag = "path"
  override def enterAppend(hull: Selection[ContainmentCluster]) {
    hull
      .style("fill", (cluster: ContainmentCluster) => baseColor(cluster.parent.id))
      .style("stroke", (cluster: ContainmentCluster) => baseColor(cluster.parent.id))
      .style("stroke-linejoin", "round")
      .style("stroke-linecap", "round")
    // .style("mix-blend-mode", "overlay")
  }

  // https://codeplea.com/introduction-to-splines
  // https://github.com/d3/d3-shape#curves
  // val curve = d3.curveCardinalClosed
  val curve = d3.curveCatmullRomClosed.alpha(0.5)
  // val curve = d3.curveLinearClosed
  // val curve = d3.curveNatural

  override def draw(hull: Selection[ContainmentCluster]) {
    hull
      .attr("d", { (cluster: ContainmentCluster) => d3.line().curve(curve)(cluster.convexHull) })
      // .style("stroke-width", (cluster: ContainmentCluster) => s"${cluster.depth * 15}px") // *2 because the stroke is half inward, half outward
      .style("opacity", (cluster: ContainmentCluster) => cluster.parent.opacity * 0.8)
  }
}

object CollapsedContainmentHullSelection extends DataSelection[ContainmentCluster] {
  override val tag = "path"
  override def enterAppend(hull: Selection[ContainmentCluster]) {
    hull
      .style("fill", (cluster: ContainmentCluster) => baseColor(cluster.parent.id))
      .style("stroke", (cluster: ContainmentCluster) => baseColor(cluster.parent.id))
      .style("stroke-linejoin", "round")
      .style("stroke-linecap", "round")
    // .style("stroke-dasharray", "10 5")
  }

  // https://codeplea.com/introduction-to-splines
  // https://github.com/d3/d3-shape#curves
  // val curve = d3.curveCardinalClosed
  val curve = d3.curveCatmullRomClosed.alpha(0.5)
  // val curve = d3.curveNatural

  override def draw(hull: Selection[ContainmentCluster]) {
    hull
      .attr("d", { (cluster: ContainmentCluster) => d3.line().curve(curve)(cluster.convexHull) })
      .style("stroke-width", (cluster: ContainmentCluster) => s"${cluster.depth * 15}px")
      .style("opacity", (cluster: ContainmentCluster) => cluster.parent.opacity * 0.4)
  }
}
