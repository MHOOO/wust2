package wust.frontend.views.graphview

import org.scalajs.d3v4._
import org.scalajs.dom.raw.HTMLElement
import rx._, rxext._
import wust.frontend._
import wust.util.collection._
import wust.util.EventTracker.sendEvent

import scalatags.JsDom.all._
import collection.breakOut

class PostRadiusSelection(graphState: GraphState, d3State: D3State) extends DataSelection[SimPost] {
  override val tag = "circle"
  override def update(post: Selection[SimPost]) {
    post
      .attr("stroke", "#888")
      .attr("fill", "transparent")
  }

  override def draw(post: Selection[SimPost]) {
    post
      .style("transform", (p: SimPost) => s"translate(${p.x.get}px,${p.y.get}px)")
      .attr("r", (p: SimPost) => p.radius)
  }
}

class PostSelection(graphState: GraphState, d3State: D3State, postDrag: PostDrag) extends DataSelection[SimPost] {
  import graphState.rxFocusedSimPost
  import postDrag._

  override val tag = "div"
  override def enter(post: Enter[SimPost]) {
    post.append((simPost: SimPost) => GraphView.postView(simPost.post)(
      pointerEvents.auto, // reenable mouse events
      cursor.default
    ).render)
      //TODO: http://bl.ocks.org/couchand/6394506 distinguish between click and doubleclick, https://stackoverflow.com/questions/42330521/distinguishing-click-and-double-click-in-d3-version-4
      //TODO: Doubleclick -> Focus
      .on("click", { (p: SimPost) =>
        DevPrintln(s"\nClicked Post: ${p.id} ${p.title}")
        Var.set(
          VarTuple(rxFocusedSimPost, rxFocusedSimPost.now.map(_.id).setOrToggle(p.id)),
          VarTuple(graphState.state.postCreatorMenus, Nil)
        )
      })
      .call(d3.drag[SimPost]()
        .clickDistance(10) // interpret short drags as clicks
        //TODO: click should not trigger drag
        .on("start", { (simPost: SimPost) =>
          Var.set(
            VarTuple(graphState.state.focusedPostId, None),
            VarTuple(graphState.state.postCreatorMenus, Nil)
          )
          postDragStarted(simPost)
        })
        .on("drag", postDragged _)
        .on("end", postDragEnded _))
  }

  override def update(post: Selection[SimPost]) {
    post
      .style("font-size", (post: SimPost) => post.fontSize)
      .style("background-color", (post: SimPost) => post.color)
      .style("border", (p: SimPost) => p.border)
      .style("opacity", (p: SimPost) => p.opacity)
      .text((p: SimPost) => p.title)

    recalculateNodeSizes(post)
    post
  }

  private def recalculateNodeSizes(post: Selection[SimPost]) {
    post.each({ (node: HTMLElement, p: SimPost) =>
      p.recalculateSize(node, d3State.transform.k)
    })
  }

  private var draw = 0
  override def draw(post: Selection[SimPost]) {

    // DevOnly {
    //   assert(post.data().forall(_.size.width == 0) || post.data().forall(_.size.width != 0))
    // }
    val onePostHasSizeZero = {
      // every drawcall exactly one different post is checked
      val simPosts = post.data()
      if (simPosts.isEmpty) false
      else simPosts(draw % simPosts.size).size.width == 0

    }
    if (onePostHasSizeZero) {
      // if one post has size zero => all posts have size zero
      // => recalculate all visible sizes
      recalculateNodeSizes(post)
    }

    post
      .style("transform", (p: SimPost) => s"translate(${p.x.get + p.centerOffset.x}px,${p.y.get + p.centerOffset.y}px)")

    draw += 1
  }
}
