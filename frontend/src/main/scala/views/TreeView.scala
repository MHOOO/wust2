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
import org.scalajs.dom.html.TextArea
import org.scalajs.dom.raw.{ Text, Element, HTMLElement, Node }
import scalatags.JsDom.all._
import scalatags.JsDom.TypedTag
import scala.scalajs.js
import scalatags.rx.all._
import scala.scalajs.js.timers.setTimeout
import org.scalajs.dom.ext.KeyCode
import org.scalajs.dom.{ Event, KeyboardEvent }

object TreeView {
  import Elements._

  implicit val postOrdering = new Ordering[Post] {
    def compare(a: Post, b: Post) = Tag.unwrap(a.id) compare Tag.unwrap(b.id)
  }

  private var setPostFocus: Option[() => Unit] = None
  private var treeContext: TreeContext[Post] = new TreeContext[Post]()

  // preserves newlines and white spaces: white-space: pre
  def textfield = div(contenteditable := "true", style := "white-space: pre", width := "80ex")

  def movePoint(postId: PostId) = div(
    paddingLeft := "5px", paddingRight := "5px", cursor := "pointer",
    span("☰"),
    ondragstart := { (e: Event) =>
      console.log("dend", e)
    },
    ondragend := { (e: Event) =>
      console.log("dstart", e)
    },
    ondrop := { (e: Event) =>
      console.log("drobbing", e)
    }
  )

  def bulletPoint(state: GlobalState, postId: PostId) = div(
    paddingLeft := "5px", paddingRight := "5px", cursor := "pointer",
    "•",
    onclick := { () => state.graphSelection() = GraphSelection.Union(Set(postId)) }
  )

  def collapseButton(state: GlobalState, postId: PostId)(implicit ctx: Ctx.Owner) = div(
    paddingLeft := "5px", paddingRight := "5px", cursor := "pointer",
    state.collapsedPostIds.map(ids => if (ids(postId)) "+" else "-"),
    onclick := { () => state.collapsedPostIds() = state.collapsedPostIds.now toggle postId }
  )

  def deleteButton(state: GlobalState, postId: PostId) = div(
    paddingLeft := "5px", paddingRight := "5px", cursor := "pointer",
    "✖",
    onclick := { () =>
      val containments = GraphSelection.toContainments(state.graphSelection.now, postId)
      state.persistence.addChangesEnriched(delPosts = Seq(postId), delContainments = containments)
    }
  )

  def nextInParent(elem: HTMLElement, next: HTMLElement => Option[HTMLElement]): Option[HTMLElement] = {
    val sibling = next(elem)
    sibling orElse {
      val parent = Option(elem.parentElement)
      parent.flatMap(nextInParent(_, next))
    }
  }

  def findNextTextfield(elem: HTMLElement, backwards: Boolean): Option[HTMLElement] = {
    val queried = elem.querySelectorAll("""div[contenteditable="true"]:not([disabled])""")

    if (queried.length <= 1) None
    else {
      var foundIdx: Option[Int] = None;
      for (i <- 0 until queried.length) {
        val e = queried(i).asInstanceOf[HTMLElement]
        if (e == document.activeElement)
          foundIdx = Some(i);
      }

      foundIdx.flatMap { foundIdx =>
        val offset = if (backwards) -1 else 1
        val nextIdx = (foundIdx + offset) match {
          case x if x < 0              => queried.length - 1
          case x if x > queried.length => 0
          case x                       => x
        }
        queried(nextIdx).asInstanceOf[js.UndefOr[HTMLElement]].toOption
      }
    }
  }

  def focusUp(elem: HTMLElement) = {
    nextInParent(elem.parentElement.parentElement.parentElement, findNextTextfield(_, backwards = true)).foreach(focusAndSetCursor _)
  }
  def focusDown(elem: HTMLElement) = {
    nextInParent(elem.parentElement.parentElement.parentElement, findNextTextfield(_, backwards = false)).foreach(focusAndSetCursor _)
  }

  def textAroundCursorSelection(elem: HTMLElement) = {
    val cursorRange = window.getSelection.getRangeAt(0)
    val lhs = document.createRange()
    val rhs = document.createRange()
    lhs.setStartBefore(elem)
    lhs.setEnd(cursorRange.startContainer, cursorRange.startOffset)
    rhs.setStart(cursorRange.endContainer, cursorRange.endOffset)
    rhs.setEndAfter(elem)
    (lhs.toString, rhs.toString)
  }

  def focusAndSetCursor(elem: HTMLElement) = if (document.activeElement != elem) {
    try {
      console.log("focusing elem", elem.asInstanceOf[js.Any])
      val s = window.getSelection()
      val r = document.createRange()
      r.selectNodeContents(Option(elem.firstChild).getOrElse(elem))
      r.collapse(false) // false: collapse to end, true: collapse to start

      s.removeAllRanges()
      elem.focus()

      s.addRange(r)
    } catch { case _: Throwable => } // https://github.com/tmpvar/jsdom/issues/317
  }

  def setFocus(renderHtml: RenderHtml[Tree[Post]], focusId: PostId) = {
    println("setting focus to " + focusId)
    setPostFocus = Some(() =>
      renderHtml.getHtmlNode(Tag.unwrap(focusId))
        .map(_.asInstanceOf[HTMLElement])
        .map(_.querySelector("""div[contenteditable="true"]:not([disabled])""").asInstanceOf[HTMLElement])
        .foreach(focusAndSetCursor)
    )
  }

  def handleKeyOnPost(state: GlobalState, tree: Tree[Post], event: KeyboardEvent, renderHtml: RenderHtml[Tree[Post]]) = {
    val post = tree.element
    val elem = event.target.asInstanceOf[HTMLElement]
    onKey(event) {
      case KeyCode.Enter if !event.shiftKey =>
        val (currPostText, newPostText) = textAroundCursorSelection(elem)
        val updatedPost = if (post.title != currPostText) {
          println("DIFFERENCE DETECTED: " + "old: " + post.title + ", new: " + currPostText)
          Some(post.copy(title = currPostText)) }else None
        val newPost = Post.newId(newPostText)

        //TODO: do not create empty post, create later when there is a title
        renderHtml.parent match {
          case Some(renderParent) =>
            val newContainment = Containment(renderParent.element.element.id, newPost.id)
            state.persistence.addChangesEnriched(addPosts = Set(newPost), addContainments = Set(newContainment), updatePosts = updatedPost.toSet)
          case None =>
            val selection = state.graphSelection.now
            val containments = GraphSelection.toContainments(selection, newPost.id)
            state.persistence.addChangesEnriched(addPosts = Set(newPost), addContainments = containments, updatePosts = updatedPost.toSet)
        }

        setFocus(renderHtml, newPost.id)
        false
      case KeyCode.Tab => event.shiftKey match {
        case false =>
          treeContext.previousMap.get(tree).foreach { previousTree =>
            val newContainment = Containment(previousTree.element.id, post.id)
            val delContainment = renderHtml.parent.map { renderParent =>
              Containment(renderParent.element.element.id, post.id)
            }
            state.persistence.addChanges(addContainments = Set(newContainment), delContainments = delContainment.toSet)
          }
          false
        case true =>
          for {
            parent <- renderHtml.parent
            grandParent = parent.renderHtml.parent
          } {
            val newContainments = grandParent.map(_.element) match {
              case Some(grandParent) => Set(Containment(grandParent.element.id, post.id))
              case None              => GraphSelection.toContainments(state.graphSelection.now, post.id)
            }
            val delContainment = Containment(parent.element.element.id, post.id)
            state.persistence.addChanges(addContainments = newContainments, delContainments = Set(delContainment))
          }
          false
      }
      case KeyCode.Up if !event.shiftKey && window.getSelection.rangeCount > 0 =>
        val sel = window.getSelection.getRangeAt(0)
        if (sel.collapsed && !elem.textContent.take(sel.endOffset).contains('\n')) {
          // setFocus(None)
          focusUp(elem)
          false
        } else true
      case KeyCode.Down if !event.shiftKey && window.getSelection.rangeCount > 0 =>
        val sel = window.getSelection.getRangeAt(0)
        if (sel.collapsed && !elem.textContent.drop(sel.endOffset).contains('\n')) {
          // setFocus(None)
          focusDown(elem)
          false
        } else true
      case KeyCode.Delete if !event.shiftKey && window.getSelection.rangeCount > 0 =>
        val sel = window.getSelection.getRangeAt(0)
        val textElem = Option(elem.firstChild.asInstanceOf[Text])
        if (sel.collapsed && textElem.fold(false)(text => sel.endOffset == text.length)) {
          treeContext.nextMap.get(tree).fold(true) { nextTree =>
            val nextPost = nextTree.element
            val updatedPost = post.copy(title = post.title + " " + nextPost.title)
            state.persistence.addChanges(updatePosts = Set(updatedPost), delPosts = Set(nextPost.id))
            false
          }
        } else true
      case KeyCode.Backspace if !event.shiftKey && window.getSelection.rangeCount > 0 =>
        val sel = window.getSelection.getRangeAt(0)
        if (sel.collapsed && sel.endOffset == 0) {
          treeContext.previousMap.get(tree).fold(true) { previousTree =>
            val prevPost = previousTree.element
            val (_, remainingText) = textAroundCursorSelection(elem)
            val updatedPost = prevPost.copy(title = prevPost.title + " " + remainingText)
            // setFocus(None)
            focusUp(elem)
            state.persistence.addChanges(updatePosts = Set(updatedPost), delPosts = Set(post.id))
            false
          }
        } else true
    }
  }

  def postItem(state: GlobalState, tree: Tree[Post], renderHtml: RenderHtml[Tree[Post]])(implicit ctx: Ctx.Owner) = {
    val post = tree.element
    val area = textfield(
      post.title,
      onfocus := { (event: Event) =>
        setFocus(renderHtml, post.id)
      },
      onblur := { (event: Event) =>
        val elem = event.target.asInstanceOf[HTMLElement]
        if (post.title != elem.textContent) {
          println("DIFFERENCE DETECTED: " + "old: " + post.title + ", new: " + elem.textContent)
          val updatedPost = post.copy(title = elem.textContent)
          state.persistence.addChanges(updatePosts = Set(updatedPost))
        }
      },
      onkeydown := { (e: KeyboardEvent) =>
        handleKeyOnPost(state, tree, e, renderHtml)
      }
    ).render

    div(
      paddingLeft := "10px",
      div(
        display.flex,
        collapseButton(state, post.id),
        bulletPoint(state, post.id),
        area,
        movePoint(post.id),
        deleteButton(state, post.id)
      )
    )
  }

  def newPostItem(state: GlobalState) = {
    val area = textfield(
      "",
      onblur := { (event: Event) =>
        val elem = event.target.asInstanceOf[HTMLElement]
        val text = elem.textContent
        if (text.nonEmpty) {
          val addPost = Post.newId(text)
          state.persistence.addChangesEnriched(addPosts = Set(addPost))
        }
      },
      onkeydown := { (event: KeyboardEvent) =>
        onKey(event) {
          case KeyCode.Enter if !event.shiftKey =>
            event.target.asInstanceOf[HTMLElement].blur()
            false
        }
      }
    ).render


    println("setting focus to new one")
    setPostFocus = Some(() => focusAndSetCursor(area))

    div(
      display.flex,
      "create new post: ",
      area
    )
  }

  def apply(state: GlobalState)(implicit ctx: Ctx.Owner) = {
    val content = div(padding := "100px").render

    implicit val canBuildFromTree = new CanBuildNestedHtml[Tree[Post]] {
      def toId(t: Tree[Post]) = Tag.unwrap(t.element.id)
      def toHtml(t: Tree[Post], render: RenderHtml[Tree[Post]]) = postItem(state, t, render).render
      def isEqual(t1: Tree[Post], t2: Tree[Post]) = t1.element.id == t2.element.id && t1.element.title == t2.element.title
      def children(element: Tree[Post]) = element.children
    }

    val renderHtml = new RenderNestedHtml[Tree[Post]](
      new RenderHtmlList[Tree[Post]](content, placeholder = () => Some(newPostItem(state).render)),
      (html, parent) => new RenderHtmlList[Tree[Post]](html, parent = Some(parent))
    )

    state.displayGraphWithoutParents.foreach { dg =>
      import dg.graph
      val rootPosts = graph.posts
        .filter(p => graph.parents(p.id).isEmpty)
        .toList.sorted

      val trees = rootPosts.map(redundantSpanningTree[Post](_, (post: Post) => graph.children(post.id).map(graph.postsById(_)).toList.sorted))

      //sideEffect: set treeContext
      treeContext = new TreeContext(trees: _*)

      renderHtml.update(trees)

      setTimeout(60)(setPostFocus.foreach(_()))
    }

    div(content)
  }
}
