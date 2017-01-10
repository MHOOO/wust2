package object graph {
  //TODO: different types of ids to restrict Connects in/out
  //TODO: this also needs to be done as database contstraint
  type AtomId = Long

  // Database layout:
  // atoms(id)
  // posts(atomid, title)
  // respondsto(atomid)
  // containment(atomid)
  // incidences(atomid, in, out)
  //    costraints:
  //      keine atomids für posts
  //      wenn respondsto: in = post, out = post or respondsto
  //      wenn containment: in = post, out = post
  case class Graph(
    posts: Map[AtomId, Post] = Map.empty,
    respondsTos: Map[AtomId, Connects] = Map.empty, //TODO: rename: responding, responses?
    containment: Map[AtomId, Contains] = Map.empty
  ) {
    //TODO: acceleration Datastructures from pharg
    def respondsToDegree(post: Post) = respondsTos.values.count(r => r.sourceId == post.id || r.targetId == post.id)
    def containsDegree(post: Post) = containment.values.count(c => c.parent == post.id || c.child == post.id)
    def fullDegree(post: Post) = respondsToDegree(post) + containsDegree(post)
    def fullDegree(respondsTo: Connects) = 2

    def incidentRespondsTos(atomId: AtomId) = respondsTos.values.collect { case r if r.sourceId == atomId || r.targetId == atomId => r.id }
    def incidentContains(atomId: AtomId) = containment.values.collect { case c if c.parent == atomId || c.child == atomId => c.id }

    def dependingRespondsEdges(atomId: AtomId) = {
      // Connects.in must be a Post, so no cycles can occour

      var next = incidentRespondsTos(atomId).toList
      var result: List[AtomId] = Nil
      var i = 0
      while (next.nonEmpty && i < 10) {
        result ::= next.head
        val candidates = incidentRespondsTos(next.head).toList
        next = next.tail ::: candidates
        i += 1
      }
      result
    }

    def remove(atomId: AtomId) = {
      val removedPosts = posts.get(atomId).map(_.id)
      val removedResponds = dependingRespondsEdges(atomId)
      val removedContains = incidentContains(atomId)
      copy(
        posts = posts -- removedPosts,
        respondsTos = respondsTos -- removedResponds,
        containment = containment -- removedContains
      )
    }

    def children(post: Post): Set[Post] = containment.values.collect { case c if c.parent == post.id => posts(c.child) }.toSet //TODO: breakout with generic on requested collection type
  }
  object Graph {
    def empty = Graph()
  }

  //TODO: rename Post -> ???
  case class Post(id: AtomId, title: String) extends PostPlatformSpecificExtensions
  object Post { def apply(title: String): Post = Post(0L, title) }
  case class Connects(id: AtomId, sourceId: AtomId, targetId: AtomId) extends RespondsToPlatformSpecificExtensions
  object Connects { def apply(in: AtomId, out: AtomId): Connects = Connects(0L, in, out) }
  //TODO: reverse direction of contains?
  case class Contains(id: AtomId, parent: AtomId, child: AtomId) extends ContainsPlatformSpecificExtensions
  object Contains { def apply(parent: AtomId, child: AtomId): Contains = Contains(0L, parent, child) }
}
