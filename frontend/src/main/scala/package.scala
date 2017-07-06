package wust

import wust.graph.Post
import wust.ids.PostId

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport

package object frontend {
  implicit class RichPostFactory(val postFactory: Post.type) extends AnyVal {
    def newId(title: String) = {
      val id = Cuid()
      postFactory.apply(PostId(id), title)
    }
  }
}
