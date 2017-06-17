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
import org.scalajs.dom.raw.{ Text, Element, HTMLElement }
import scalatags.JsDom.all._
import scala.scalajs.js
import scalatags.rx.all._
import scala.scalajs.js.timers.setTimeout
import org.scalajs.dom.ext.KeyCode
import org.scalajs.dom.{ DragEvent, Event, KeyboardEvent }

//TODO proper ordering and move to wust.ids
object PostOrdering extends Ordering[Post] {
  def compare(a: Post, b: Post): Int = Tag.unwrap(a.id) compare Tag.unwrap(b.id)
}

object TreeView {
  import Elements._

  //TODO better?
  private var focusedPostId: Option[PostId] = None

  private implicit val postOrdering = PostOrdering

  private object DataKeys {
    val postId = "postId"
    val prevPostId = "prevPostId"
  }

  // preserves newlines and white spaces: white-space: pre
  def textfield = div(contenteditable := "true", style := "white-space: pre", width := "80ex")

  def movePoint(state: GlobalState, c: TreeContext[Post], tree: Tree[Post])(implicit ctx: Ctx.Owner) = {
    val post = tree.element
    val prevId = c.previousMap.get(tree).map(_.element.id)
    val isDragged = Var(false)

    div(
      paddingLeft := "5px", paddingRight := "5px", cursor := "pointer",
      color := Rx { if (isDragged()) "blue" else "inherit" },
      "☰",
      draggable := true,
      ondragstart := { (e: DragEvent) =>
        e.dataTransfer.setData(DataKeys.postId, Tag.unwrap(post.id))
        e.dataTransfer.setData(DataKeys.prevPostId, prevId.map(Tag.unwrap _).getOrElse(""))
        isDragged() = true
      },
      ondragend := { (e: DragEvent) =>
        e.dataTransfer.clearData(DataKeys.postId)
        e.dataTransfer.clearData(DataKeys.prevPostId)
        isDragged() = false
      }
    )
  }

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
    nextInParent(elem, findNextTextfield(_, backwards = true)).foreach(focusAndSetCursor _)
  }
  def focusDown(elem: HTMLElement) = {
    nextInParent(elem, findNextTextfield(_, backwards = false)).foreach(focusAndSetCursor _)
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

  def focusAndSetCursor(elem: HTMLElement) {
    try {
      elem.focus()
      val s = window.getSelection()
      val r = document.createRange()
      r.selectNodeContents(Option(elem.firstChild).getOrElse(elem))
      r.collapse(false) // false: collapse to end, true: collapse to start
      s.removeAllRanges()
      s.addRange(r)
    } catch { case _: Throwable => } // https://github.com/tmpvar/jsdom/issues/317
  }

  def handleKey(state: GlobalState, c: TreeContext[Post], tree: Tree[Post], event: KeyboardEvent) = {
    val post = tree.element
    val elem = event.target.asInstanceOf[HTMLElement]
    onKey(event) {
      case KeyCode.Enter if !event.shiftKey =>
        val (currPostText, newPostText) = textAroundCursorSelection(elem)
        val updatedPost = if (post.title != currPostText) Some(post.copy(title = currPostText)) else None
        val newPost = Post.newId(newPostText)
        //TODO: do not create empty post, create later when there is a title
        focusedPostId = Some(newPost.id)
        val containments = c.parentMap.get(tree) match {
          case Some(parentTree) => Set(Containment(parentTree.element.id, newPost.id))
          case None             => GraphSelection.toContainments(state.graphSelection.now, newPost.id)
        }

        state.persistence.addChangesEnriched(addPosts = Set(newPost), addContainments = containments, updatePosts = updatedPost.toSet)
        false
      case KeyCode.Tab if !event.shiftKey =>
        c.previousMap.get(tree).foreach { previousTree =>
          val newContainment = Containment(previousTree.element.id, post.id)
          val delContainment = c.parentMap.get(tree).map { parentTree =>
            Containment(parentTree.element.id, post.id)
          }
          focusedPostId = Some(post.id)
          state.persistence.addChanges(addContainments = Set(newContainment), delContainments = delContainment.toSet)
        }
        false
      case KeyCode.Tab if event.shiftKey =>
        for {
          parent <- c.parentMap.get(tree)
          grandParent = c.parentMap.get(parent)
        } {
          val newContainments = grandParent match {
            case Some(grandParent) => Set(Containment(grandParent.element.id, post.id))
            case None              => GraphSelection.toContainments(state.graphSelection.now, post.id)
          }
          val delContainment = Containment(parent.element.id, post.id)
          focusedPostId = Some(post.id)
          state.persistence.addChanges(addContainments = newContainments, delContainments = Set(delContainment))
        }
        false
      case KeyCode.Up if !event.shiftKey =>
        val sel = window.getSelection.getRangeAt(0)
        if (sel.collapsed && !elem.textContent.take(sel.startOffset).contains('\n')) {
          focusedPostId = None
          focusUp(elem)
          false
        } else true
      case KeyCode.Down if !event.shiftKey =>
        val sel = window.getSelection.getRangeAt(0)
        if (sel.collapsed && !elem.textContent.drop(sel.endOffset).contains('\n')) {
          focusedPostId = None
          focusDown(elem)
          false
        } else true
      case KeyCode.Delete if !event.shiftKey =>
        val sel = window.getSelection.getRangeAt(0)
        val textElem = elem.firstChild.asInstanceOf[Text]
        if (sel.collapsed && sel.endOffset == textElem.length) {
          val target = c.nextMap.get(tree).orElse(tree.children.headOption)
          target.map { targetTree =>
            val targetPost = targetTree.element
            val updatedPost = post.copy(title = post.title + " " + targetPost.title)
            val childPosts = targetTree.children.map(_.element)
            val containments = childPosts.map(p => Containment(post.id, p.id))
            focusedPostId = Some(post.id)
            state.persistence.addChanges(addContainments = containments.toSet, updatePosts = Set(updatedPost), delPosts = Set(targetPost.id))
            false
          }.getOrElse(true)
        } else true
      case KeyCode.Backspace if !event.shiftKey =>
        val sel = window.getSelection.getRangeAt(0)
        if (sel.collapsed && sel.endOffset == 0) {
          val target = c.previousMap.get(tree).orElse(c.parentMap.get(tree))
          target.map { targetTree =>
            val targetPost = targetTree.element
            val (_, remainingText) = textAroundCursorSelection(elem)
            val updatedPost = targetPost.copy(title = targetPost.title + " " + remainingText)
            val childPosts = tree.children.map(_.element)
            val containments = childPosts.map(p => Containment(targetPost.id, p.id))
            focusedPostId = None
            focusUp(elem)
            state.persistence.addChanges(addContainments = containments.toSet, updatePosts = Set(updatedPost), delPosts = Set(post.id))
            false
          }.getOrElse(true)
        } else true
    }
  }

  def postItem(state: GlobalState, c: TreeContext[Post], tree: Tree[Post])(implicit ctx: Ctx.Owner) = {
    val post = tree.element
    val area = textfield(
      post.title,
      onfocus := { () =>
        if (focusedPostId.isEmpty)
          focusedPostId = Some(post.id)
      },
      onblur := { (event: Event) =>
        val elem = event.target.asInstanceOf[HTMLElement]
        if (post.title != elem.textContent) {
          val updatedPost = post.copy(title = elem.textContent)
          state.persistence.addChanges(updatePosts = Set(updatedPost))
        }
      },
      onkeydown := { (e: KeyboardEvent) => handleKey(state, c, tree, e) }
    ).render

    //TODO: better?
    if (focusedPostId.map(_ == post.id).getOrElse(false)) {
      setTimeout(60) { focusAndSetCursor(area) }
    }

    val isHovered = Var(false)
    val hoverPlaceholder = div(
      width := "40px", height := "2px", backgroundColor := "blue",
      display := Rx { if (isHovered()) "block" else "none" }
    )

    div(
      div(
        display.flex,
        collapseButton(state, post.id),
        bulletPoint(state, post.id),
        area,
        movePoint(state, c, tree),
        deleteButton(state, post.id)
      ),
      hoverPlaceholder,
      ondragenter := { (e: Event) =>
        isHovered() = true
      },
      ondragleave := { (e: Event) =>
        isHovered() = false
      },
      ondragover := { (e: DragEvent) =>
        e.preventDefault() //needed for dropping
        e.dataTransfer.dropEffect = "move"
        false
      },
      ondrop := { (e: DragEvent) =>
        e.preventDefault()
        console.log("drobbing", e)
        val targetId = PostId(e.dataTransfer.getData(DataKeys.postId))
        val prevId = e.dataTransfer.getData(DataKeys.prevPostId) match {
          case "" => None
          case id => Some(PostId(id))
        }
        val connection = Connection(targetId, post.id)
        val toDelete = prevId.map(Connection(targetId, _)).toSet
        state.persistence.addChanges(addConnections = Set(connection), delConnections = toDelete)
        false
      }
    )
  }

  def newItem(state: GlobalState)(implicit ctx: Ctx.Owner): Frag = {
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

    setTimeout(60) { focusAndSetCursor(area) }

    div(
      "create new post: ",
      area
    )
  }

  def postTreeItem(state: GlobalState, c: TreeContext[Post], tree: Tree[Post])(implicit ctx: Ctx.Owner): Frag = {
    val childNodes = tree.children
      .sortBy(_.element)
      .map(postTreeItem(state, c, _))
      .toList

    postItem(state, c, tree)(ctx)(
      paddingLeft := "10px",
      childNodes
    )
  }

  def apply(state: GlobalState)(implicit ctx: Ctx.Owner) = {
    div(state.displayGraph.map { dg =>
      import dg.graph

      def postChildren(post: Post): Iterable[Post] = {
        graph.children(post.id).map(graph.postsById(_))
      }

      val rootPosts = graph.posts
        .filter(p => graph.parents(p.id).isEmpty)
        .toList
        .sorted

      val trees = rootPosts.map(redundantSpanningTree[Post](_, postChildren _))
      val context = new TreeContext(trees: _*)

      val items = trees.map(postTreeItem(state, context, _))
      div(
        padding := "100px",
        if (items.isEmpty) Seq(newItem(state)) else items
      ).render
    })
  }
}
