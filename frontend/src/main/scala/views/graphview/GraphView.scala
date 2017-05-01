package wust.frontend.views.graphview

import org.scalajs.d3v4._
import org.scalajs.dom
import rx._
import wust.frontend.Color._
import wust.frontend.{DevOnly, GlobalState}
import wust.graph._
import wust.util.Pipe

import scala.scalajs.js
import scala.scalajs.js.JSConverters._
import scalatags.JsDom.all._

case class MenuAction(name: String, action: (SimPost, Simulation[SimPost]) => Unit)
case class DropAction(name: String, action: (SimPost, SimPost) => Unit)

object KeyImplicits {
  implicit val SimPostWithKey = new WithKey[SimPost](_.id)
  implicit val SimConnectionWithKey = new WithKey[SimConnection](_.id)
  implicit val SimRedirectedConnectionWithKey = new WithKey[SimRedirectedConnection](c => s"${c.sourceId} ${c.targetId}")
  implicit val ContainmentClusterWithKey = new WithKey[ContainmentCluster](_.id)
}

class GraphView(state: GlobalState, element: dom.html.Element, disableSimulation: Boolean = false)(implicit ctx: Ctx.Owner) {
  val graphState = new GraphState(state)
  val d3State = new D3State(disableSimulation)
  val postDrag = new PostDrag(graphState, d3State, onPostDrag)
  import state.{displayGraph => rxDisplayGraph, _}
  import graphState._

  // prepare containers where we will append elements depending on the data
  // order is important
  import KeyImplicits._
  val container = d3.select(element)
  val svg = container.append("svg")
  val containmentHullSelection = SelectData.rx(ContainmentHullSelection, rxContainmentCluster)(svg.append("g"))
  val collapsedContainmentHullSelection = SelectData.rx(CollapsedContainmentHullSelection, rxCollapsedContainmentCluster)(svg.append("g"))
  val connectionLineSelection = SelectData.rx(ConnectionLineSelection, rxSimConnection)(svg.append("g"))
  val redirectedConnectionLineSelection = SelectData.rx(RedirectedConnectionLineSelection, rxSimRedirectedConnection)(svg.append("g"))

  val html = container.append("div")
  val connectionElementSelection = SelectData.rx(ConnectionElementSelection, rxSimConnection)(html.append("div"))
  val postSelection = SelectData.rx(new PostSelection(graphState, d3State, postDrag), rxSimPosts)(html.append("div"))
  val draggingPostSelection = SelectData.rxDraw(DraggingPostSelection, postDrag.draggingPosts)(html.append("div")) //TODO: place above ring menu?

  val menuSvg = container.append("svg")
  val postMenuLayer = menuSvg.append("g")
  val postMenuSelection = SelectData.rxDraw(new PostMenuSelection(graphState, d3State), rxFocusedSimPost.map(_.toJSArray))(postMenuLayer.append("g"))
  val dropMenuLayer = menuSvg.append("g")
  val dropMenuSelection = SelectData.rxDraw(DropMenuSelection, postDrag.closestPosts)(dropMenuLayer.append("g"))

  initContainerDimensionsAndPositions()
  initEvents()

  // set the background according to focused parents
  Rx {
    val selection = state.graphSelection()
    val focusedParents = selection match {
      case GraphSelection.Root => Set.empty
      case GraphSelection.Union(parentIds) => parentIds
    }
    val mixedDirectParentColors = mixColors(focusedParents.map(baseColor))
    container
      .style("background-color", mixedDirectParentColors.toString)
      .style("opacity", "0.8")
  }

  Rx { rxDisplayGraph(); rxSimPosts(); rxSimConnection(); rxSimContainment() }.triggerLater {
    val simPosts = rxSimPosts.now
    val simConnection = rxSimConnection.now
    val simRedirectedConnection = rxSimRedirectedConnection.now
    val simContainment = rxSimContainment.now
    val simCollapsedContainment = rxSimCollapsedContainment.now
    // val graph = rxDisplayGraph.now.graph

    DevOnly {
      println("    updating graph simulation")
    }

    //TODO: this can be removed after implementing link force which supports hyperedges
    // the strength functions depend on the latest graph and are called
    // when setting nodes and links. Therefore they need to be set before.
    // even when updating the force.initialize method is called,
    // trying to access all posts which could not exist anymore
    // d3State.forces.connection.strength { (e: SimConnection) =>
    //   1.0 / math.min(graph.fullDegree(e.source.id), graph.fullDegree(e.target.id))
    // }

    // d3State.forces.containment.strength { (e: SimContainment) =>
    //   1.0 / math.min(graph.fullDegree(e.source.post.id), graph.fullDegree(e.target.post.id))
    // }

    d3State.simulation.nodes(simPosts)
    d3State.forces.connection.links(simConnection)
    d3State.forces.redirectedConnection.links(simRedirectedConnection)
    d3State.forces.containment.links(simContainment)
    d3State.forces.collapsedContainment.links(simCollapsedContainment)

    d3State.simulation.alpha(1).restart()
  }

  private def onPostDrag() {
    draggingPostSelection.draw()
  }

  private def initEvents(): Unit = {
    svg.call(d3.zoom().on("zoom", zoomed _))
    svg.on("click", () => focusedPostId() = None)
    d3State.simulation.on("tick", draw _)
    DevOnly { d3State.simulation.on("end", { () => println("simulation ended") }) }
  }

  private def zoomed() {
    import d3State._
    transform = d3.event.asInstanceOf[ZoomEvent].transform
    svg.selectAll("g").attr("transform", transform.toString)
    html.style("transform", s"translate(${transform.x}px,${transform.y}px) scale(${transform.k})")
    postMenuLayer.attr("transform", transform.toString)
    dropMenuLayer.attr("transform", transform.toString)
  }

  private def draw() {
    postSelection.draw()
    postMenuSelection.draw()
    connectionLineSelection.draw()
    redirectedConnectionLineSelection.draw()
    connectionElementSelection.draw()
    containmentHullSelection.draw()
    collapsedContainmentHullSelection.draw()
  }

  private def initContainerDimensionsAndPositions() {
    container
      .style("position", "absolute")
      .style("top", "0")
      .style("left", "0")
      .style("z-index", "-1")
      .style("width", "100%")
      .style("height", "100%")
      .style("overflow", "hidden")

    svg
      .style("position", "absolute")
      .style("width", "100%")
      .style("height", "100%")

    html
      .style("position", "absolute")
      .style("pointer-events", "none") // pass through to svg (e.g. zoom)
      .style("transform-origin", "top left") // same as svg default
      .style("width", "100%")
      .style("height", "100%")

    menuSvg
      .style("position", "absolute")
      .style("width", "100%")
      .style("height", "100%")
      .style("pointer-events", "none")
  }
}

object GraphView {
  def apply(state: GlobalState, disableSimulation: Boolean = false)(implicit ctx: Ctx.Owner) = {
    div(div().render ||> (new GraphView(state, _, disableSimulation)))
  }
}
