package wust.db

import org.scalatest._

import scala.concurrent.duration._
import scala.concurrent.{ Await, Future }

import wust.db.data._

// TODO: Query-Probing: https://github.com/getquill/quill#query-probing
// "Query probing validates queries against the database at compile time, failing the compilation if it is not valid. The query validation does not alter the database state."
class DbSpec extends DbIntegrationTestSpec with MustMatchers {
  implicit def passwordToDigest(pw: String): Array[Byte] = pw.map(_.toByte).toArray
  implicit class EqualityByteArray(val arr: Array[Byte]) {
    def mustEqualDigest(pw: String) = arr mustEqual passwordToDigest(pw)
  }

  "post" - {
    "create public post" in { db =>
      import db._, db.ctx, ctx._
      for {
        post <- db.post.createPublic("t")

        queriedPosts <- ctx.run(query[Post])
        queriedOwnerships <- ctx.run(query[Ownership])
      } yield {
        post.title mustEqual "t"
        queriedPosts must contain theSameElementsAs List(post)
        queriedOwnerships mustBe empty
      }
    }

    "create public post with apply" in { db =>
      import db._, db.ctx, ctx._
      for {
        (post, None) <- db.post("t", groupIdOpt = None)

        queriedPosts <- ctx.run(query[Post])
        queriedOwnerships <- ctx.run(query[Ownership])
      } yield {
        post.title mustEqual "t"
        queriedPosts must contain theSameElementsAs List(post)
        queriedOwnerships mustBe empty
      }
    }

    "create owned post" in { db =>
      import db._, db.ctx, ctx._
      for {
        // groupId <- ctx.run(query[UserGroup].insert(lift(UserGroup())).returning(_.id))
        groupId <- ctx.run(infix"insert into usergroup(id) values(DEFAULT)".as[Insert[UserGroup]].returning(_.id))
        (post, ownership) <- db.post.createOwned("t", groupId)

        queriedPosts <- ctx.run(query[Post])
        queriedOwnerships <- ctx.run(query[Ownership])
      } yield {
        post.title mustEqual "t"
        ownership mustEqual Ownership(post.id, groupId)

        queriedPosts must contain theSameElementsAs List(post)
        queriedOwnerships must contain theSameElementsAs List(Ownership(post.id, groupId))
      }
    }

    "create owned post with apply" in { db =>
      import db._, db.ctx, ctx._
      for {
        // groupId <- ctx.run(query[UserGroup].insert(lift(UserGroup())).returning(_.id))
        groupId <- ctx.run(infix"insert into usergroup(id) values(DEFAULT)".as[Insert[UserGroup]].returning(_.id))
        (post, Some(ownership)) <- post("t", Option(groupId))

        queriedPosts <- ctx.run(query[Post])
        queriedOwnerships <- ctx.run(query[Ownership])
      } yield {
        post.title mustEqual "t"
        ownership mustEqual Ownership(post.id, groupId)

        queriedPosts must contain theSameElementsAs List(post)
        queriedOwnerships must contain theSameElementsAs List(Ownership(post.id, groupId))
      }
    }

    "get existing post" in { db =>
      import db._, db.ctx, ctx._
      for {
        Post(postId, _) <- db.post.createPublic("t")
        getPost <- db.post.get(postId)
      } yield {
        getPost mustEqual Option(Post(postId, "t"))
      }
    }

    "get non-existing post" in { db =>
      import db._, db.ctx, ctx._
      for {
        getPost <- db.post.get(17134)
      } yield {
        getPost mustEqual None
      }
    }

    "update existing post" in { db =>
      import db._, db.ctx, ctx._
      for {
        post <- db.post.createPublic("t")
        updatedPost <- db.post.update(post.copy(title = "harals"))
        queriedPosts <- ctx.run(query[Post])
      } yield {
        updatedPost mustBe true
        queriedPosts must contain theSameElementsAs List(post.copy(title = "harals"))
      }
    }

    "update non-existing post" in { db =>
      import db._, db.ctx, ctx._
      for {
        updatedPost <- db.post.update(Post(1135, "harals"))
        queriedPosts <- ctx.run(query[Post])
      } yield {
        updatedPost mustBe false
        queriedPosts mustBe empty
      }
    }

    "delete existing post" in { db =>
      import db._, db.ctx, ctx._
      for {
        post <- db.post.createPublic("t")
        deleted <- db.post.delete(post.id)
        queriedPosts <- ctx.run(query[Post])
      } yield {
        deleted mustBe true
        queriedPosts mustBe empty
      }
    }

    "delete non-existing post" in { db =>
      import db._, db.ctx, ctx._
      for {
        deleted <- db.post.delete(135481)
        queriedPosts <- ctx.run(query[Post])
      } yield {
        deleted mustBe false
        queriedPosts mustBe empty
      }
    }

    "get parentIds" in { db =>
      import db._, db.ctx, ctx._
      for {
        child <- db.post.createPublic("Sohnemann")
        parent1 <- db.post.createPublic("Borolf")
        parent2 <- db.post.createPublic("Migrota")
        Some(_) <- db.containment(parent1.id, child.id)
        Some(_) <- db.containment(parent2.id, child.id)
        parentIds <- db.post.getParentIds(child.id)
      } yield {
        parentIds must contain theSameElementsAs List(parent1.id, parent2.id)
      }
    }
  }

  "connection" - {
    "create between two existing posts" in { db =>
      import db._, db.ctx, ctx._
      for {
        sourcePost <- db.post.createPublic("s")
        targetPost <- db.post.createPublic("t")
        Some(connection) <- db.connection(sourcePost.id, targetPost.id)
        connections <- ctx.run(query[Connection])
      } yield {
        connection.sourceId mustEqual sourcePost.id
        connection.targetId mustEqual targetPost.id
        connections must contain theSameElementsAs List(connection)
      }
    }

    "create between two existing posts with already existing connection" in { db =>
      import db._, db.ctx, ctx._
      for {
        sourcePost <- db.post.createPublic("s")
        targetPost <- db.post.createPublic("t")
        Some(_) <- db.connection(sourcePost.id, targetPost.id)
        Some(connection) <- db.connection(sourcePost.id, targetPost.id)
        connections <- ctx.run(query[Connection])
      } yield {
        connection.sourceId mustEqual sourcePost.id
        connection.targetId mustEqual targetPost.id
        connections must contain theSameElementsAs List(connection)
      }
    }

    "create between two existing posts with already existing containments" in { db =>
      import db._, db.ctx, ctx._
      for {
        sourcePost <- db.post.createPublic("s")
        targetPost <- db.post.createPublic("t")
        Some(_) <- db.containment(sourcePost.id, targetPost.id)
        Some(_) <- db.containment(targetPost.id, sourcePost.id)
        Some(connection) <- db.connection(sourcePost.id, targetPost.id)
        connections <- ctx.run(query[Connection])
      } yield {
        connection.sourceId mustEqual sourcePost.id
        connection.targetId mustEqual targetPost.id
        connections must contain theSameElementsAs List(connection)
      }
    }

    "create between two posts, source not existing" in { db =>
      import db._, db.ctx, ctx._
      for {
        targetPost <- db.post.createPublic("t")
        connectionOpt <- db.connection(131565, targetPost.id)
        connections <- ctx.run(query[Connection])
      } yield {
        connectionOpt mustEqual None
        connections mustBe empty
      }
    }

    "create between two posts, target not existing" in { db =>
      import db._, db.ctx, ctx._
      for {
        sourcePost <- db.post.createPublic("s")
        connectionOpt <- db.connection(sourcePost.id, 131565)
        connections <- ctx.run(query[Connection])
      } yield {
        connectionOpt mustEqual None
        connections mustBe empty
      }
    }

    "create between two posts, both not existing" in { db =>
      import db._, db.ctx, ctx._
      for {
        connectionOpt <- db.connection(16816, 131565)
        connections <- ctx.run(query[Connection])
      } yield {
        connectionOpt mustEqual None
        connections mustBe empty
      }
    }

    // TODO: reactivate when hyperedges are back
    // "create from post to other connection" in { db =>
    //   import db._, db.ctx, ctx._
    //   for {
    //     sourcePost <- db.post.createPublic("s")
    //     aPost <- db.post.createPublic("a")
    //     bPost <- db.post.createPublic("b")
    //     Some(targetConnection) <- db.connection(aPost.id, bPost.id)

    //     Some(connection) <- db.connection(sourcePost.id, targetConnection.id)
    //     connections <- ctx.run(query[Connection])
    //   } yield {
    //     connection.sourceId mustEqual sourcePost.id
    //     connection.targetId mustEqual targetConnection.id
    //     connections must contain theSameElementsAs List(targetConnection, connection)
    //   }
    // }

    "create connection with new public post" in { db =>
      import db._, db.ctx, ctx._
      for {
        targetPost <- db.post.createPublic("t")

        Some((post, connection, None)) <- db.connection.newPost("response", targetPost.id, groupIdOpt = None)

        queriedPosts <- ctx.run(query[Post])
        queriedConnections <- ctx.run(query[Connection])
        queriedOwnerships <- ctx.run(query[Ownership])
      } yield {
        post.title mustEqual "response"
        connection.targetId mustEqual targetPost.id
        connection.sourceId mustEqual post.id

        queriedPosts must contain theSameElementsAs List(targetPost, post)
        queriedConnections must contain theSameElementsAs List(connection)
        queriedOwnerships mustBe empty
      }
    }

    "create connection with new owned post" in { db =>
      import db._, db.ctx, ctx._
      for {
        targetPost <- db.post.createPublic("t")
        groupId <- ctx.run(infix"insert into usergroup(id) values(DEFAULT)".as[Insert[UserGroup]].returning(_.id))

        Some((post, connection, Some(ownership))) <- db.connection.newPost("response", targetPost.id, groupIdOpt = Option(groupId))

        queriedPosts <- ctx.run(query[Post])
        queriedConnections <- ctx.run(query[Connection])
        queriedOwnerships <- ctx.run(query[Ownership])
      } yield {
        post.title mustEqual "response"
        connection.targetId mustEqual targetPost.id
        connection.sourceId mustEqual post.id

        queriedPosts must contain theSameElementsAs List(targetPost, post)
        queriedConnections must contain theSameElementsAs List(connection)
        queriedOwnerships must contain theSameElementsAs List(Ownership(post.id, groupId))
      }
    }

    "create connection with new public post to non-existing targetId" in { db =>
      import db._, db.ctx, ctx._
      for {
        connectionResultOpt <- db.connection.newPost("response", 612345, groupIdOpt = None)

        queriedPosts <- ctx.run(query[Post])
      } yield {
        connectionResultOpt mustEqual None
        queriedPosts mustBe empty
      }
    }

    "delete existing connection" in { db =>
      import db._, db.ctx, ctx._
      for {
        sourcePost <- db.post.createPublic("s")
        targetPost <- db.post.createPublic("t")
        Some(connection) <- db.connection(sourcePost.id, targetPost.id)

        deleted <- db.connection.delete(connection)
      } yield {
        deleted mustEqual true
      }
    }

    "delete non-existing connection" in { db =>
      import db._, db.ctx, ctx._
      for {
        deleted <- db.connection.delete(Connection(165151, 15615))
      } yield {
        deleted mustEqual false
      }
    }
  }

  "containment" - {
    "create between two existing posts" in { db =>
      import db._, db.ctx, ctx._
      for {
        parentPost <- db.post.createPublic("s")
        childPost <- db.post.createPublic("t")
        Some(containment) <- db.containment(parentPost.id, childPost.id)
        containments <- ctx.run(query[Containment])
      } yield {
        containment.parentId mustEqual parentPost.id
        containment.childId mustEqual childPost.id
        containments must contain theSameElementsAs List(containment)
      }
    }

    "create between two existing posts with already existing containment" in { db =>
      import db._, db.ctx, ctx._
      for {
        parentPost <- db.post.createPublic("s")
        childPost <- db.post.createPublic("t")
        Some(_) <- db.containment(parentPost.id, childPost.id)
        Some(containment) <- db.containment(parentPost.id, childPost.id)
        containments <- ctx.run(query[Containment])
      } yield {
        containment.parentId mustEqual parentPost.id
        containment.childId mustEqual childPost.id
        containments must contain theSameElementsAs List(containment)
      }
    }

    "create between two existing posts with already existing connection" in { db =>
      import db._, db.ctx, ctx._
      for {
        parentPost <- db.post.createPublic("s")
        childPost <- db.post.createPublic("t")
        Some(_) <- db.connection(parentPost.id, childPost.id)
        Some(_) <- db.connection(childPost.id, parentPost.id)
        Some(containment) <- db.containment(parentPost.id, childPost.id)
        containments <- ctx.run(query[Containment])
      } yield {
        containment.parentId mustEqual parentPost.id
        containment.childId mustEqual childPost.id
        containments must contain theSameElementsAs List(containment)
      }
    }

    "create between two posts, parent not existing" in { db =>
      import db._, db.ctx, ctx._
      for {
        childPost <- db.post.createPublic("t")
        containmentOpt <- db.containment(131565, childPost.id)
        containments <- ctx.run(query[Containment])
      } yield {
        containmentOpt mustEqual None
        containments mustBe empty
      }
    }

    "create between two posts, child not existing" in { db =>
      import db._, db.ctx, ctx._
      for {
        parentPost <- db.post.createPublic("s")
        containmentOpt <- db.containment(parentPost.id, 131565)
        containments <- ctx.run(query[Containment])
      } yield {
        containmentOpt mustEqual None
        containments mustBe empty
      }
    }

    "create between two posts, both not existing" in { db =>
      import db._, db.ctx, ctx._
      for {
        containmentOpt <- db.containment(16816, 131565)
        containments <- ctx.run(query[Containment])
      } yield {
        containmentOpt mustEqual None
        containments mustBe empty
      }
    }

    "delete existing containment" in { db =>
      import db._, db.ctx, ctx._
      for {
        parentPost <- db.post.createPublic("s")
        childPost <- db.post.createPublic("t")
        Some(containment) <- db.containment(parentPost.id, childPost.id)

        deleted <- db.containment.delete(containment)
        containments <- ctx.run(query[Containment])
      } yield {
        deleted mustEqual true
        containments mustBe empty
      }
    }

    "delete non-existing containment" in { db =>
      import db._, db.ctx, ctx._
      for {
        deleted <- db.containment.delete(Containment(165151, 16851))
        containments <- ctx.run(query[Containment])
      } yield {
        deleted mustEqual false
        containments mustBe empty
      }
    }
  }

  "user" - {
    "create non-existing" in { db =>
      import db._, db.ctx, ctx._
      for {
        Some(user) <- db.user("heigo", "parwin")
        Some((queriedUser, queriedDigest)) <- db.user.getUserAndDigest("heigo")
        queriedGroups <- ctx.run(query[UserGroup])
      } yield {
        user.name mustEqual "heigo"
        user.isImplicit mustEqual false
        user.revision mustEqual 0
        queriedUser mustEqual user
        queriedDigest mustEqualDigest "parwin"
        queriedGroups mustBe empty
      }
    }

    "try to create existing with same password" in { db =>
      import db._, db.ctx, ctx._
      for {
        Some(existingUser) <- db.user("heigo", "parwin")
        None <- db.user("heigo", "parwin")
        Some((queriedUser, queriedDigest)) <- db.user.getUserAndDigest("heigo")
      } yield {
        queriedUser mustEqual existingUser
        queriedDigest mustEqualDigest "parwin"
      }
    }

    "try to create existing with different password" in { db =>
      import db._, db.ctx, ctx._
      for {
        Some(existingUser) <- db.user("heigo", "parwin")
        None <- db.user("heigo", "reidon")
        Some((queriedUser, queriedDigest)) <- db.user.getUserAndDigest("heigo")
      } yield {
        queriedUser mustEqual existingUser
        queriedDigest mustEqualDigest "parwin"
      }
    }

    "create implicit user" in { db =>
      import db._, db.ctx, ctx._
      for {
        user <- db.user.createImplicitUser()
        queriedUsers <- ctx.run(query[User])
        queriedPasswords <- ctx.run(query[Password])
      } yield {
        user.name must startWith("anon-")
        user.isImplicit mustEqual true
        user.revision mustEqual 0
        queriedUsers must contain theSameElementsAs List(user)
        queriedPasswords mustBe empty
      }
    }

    "create two implicit users" in { db =>
      import db._, db.ctx, ctx._
      for {
        user1 <- db.user.createImplicitUser()
        user2 <- db.user.createImplicitUser()
        queriedUsers <- ctx.run(query[User])
        queriedPasswords <- ctx.run(query[Password])
      } yield {
        user1.name must not equal (user2.name)
        queriedUsers.toSet mustEqual Set(user1, user2)
        queriedPasswords mustBe empty
      }
    }

    "activate implicit user to non-existing" in { db =>
      import db._, db.ctx, ctx._
      for {
        implUser <- db.user.createImplicitUser()
        Some(user) <- db.user.activateImplicitUser(implUser.id, "ganiz", "faura")
        Some((queriedUser, queriedDigest)) <- db.user.getUserAndDigest("ganiz")
      } yield {
        user.name mustEqual "ganiz"
        user.isImplicit mustEqual false
        user.revision mustEqual 1
        queriedUser mustEqual user
        queriedDigest mustEqualDigest "faura"
      }
    }

    "try to activate implicit user to existing with same password" in { db =>
      import db._, db.ctx, ctx._
      for {
        implUser <- db.user.createImplicitUser()
        Some(existingUser) <- db.user("ganiz", "heuriso")
        None <- db.user.activateImplicitUser(implUser.id, "ganiz", "heuriso")
        Some((queriedUser, queriedDigest)) <- db.user.getUserAndDigest("ganiz")
        queriedUsers <- ctx.run(query[User])
        queriedPasswords <- ctx.run(query[Password])
      } yield {
        queriedUsers must contain theSameElementsAs List(existingUser, implUser)
        queriedPasswords.size mustEqual 1
        queriedUser mustEqual existingUser
        queriedDigest mustEqualDigest "heuriso"
      }
    }

    "try to activate implicit user to existing with different password" in { db =>
      import db._, db.ctx, ctx._
      for {
        implUser <- db.user.createImplicitUser()
        Some(existingUser) <- db.user("ganiz", "heuriso")
        None <- db.user.activateImplicitUser(implUser.id, "ganiz", "faura")
        Some((queriedUser, queriedDigest)) <- db.user.getUserAndDigest("ganiz")
        queriedUsers <- ctx.run(query[User])
        queriedPasswords <- ctx.run(query[Password])
      } yield {
        queriedUsers must contain theSameElementsAs List(existingUser, implUser)
        queriedPasswords.size mustEqual 1
        queriedUser mustEqual existingUser
        queriedDigest mustEqualDigest "heuriso"
      }
    }

    "get existing by id" in { db =>
      import db._, db.ctx, ctx._
      for {
        Some(existingUser) <- db.user("heigo", "parwin")
        Some(user) <- db.user.get(existingUser.id)
      } yield {
        user mustEqual existingUser
      }
    }

    "get non-existing by id" in { db =>
      import db._, db.ctx, ctx._
      for {
        userOpt <- db.user.get(11351)
      } yield {
        userOpt mustEqual None
      }
    }

    "get existing by name,password" in { db =>
      import db._, db.ctx, ctx._
      for {
        Some(existingUser) <- db.user("heigo", "parwin")
        Some((user, digest)) <- db.user.getUserAndDigest("heigo")
      } yield {
        digest mustEqualDigest "parwin"
        user mustEqual existingUser
      }
    }

    "get non-existing by name,password" in { db =>
      import db._, db.ctx, ctx._
      for {
        userOpt <- db.user.getUserAndDigest("a")
      } yield {
        userOpt mustEqual None
      }
    }

    "get existing with wrong username" in { db =>
      import db._, db.ctx, ctx._
      for {
        Some(existingUser) <- db.user("heigo", "parwin")
        userOpt <- db.user.getUserAndDigest("ürgens")
      } yield {
        userOpt mustEqual None
      }
    }

    "check if existing user exists" in { db =>
      import db._, db.ctx, ctx._
      for {
        Some(existingUser) <- db.user("heigo", "parwin")
        exists <- db.user.checkIfEqualUserExists(existingUser)
      } yield {
        exists mustBe true
      }
    }

    "check if existing user exists (wrong id)" in { db =>
      import db._, db.ctx, ctx._
      for {
        Some(existingUser) <- db.user("heigo", "parwin")
        exists <- db.user.checkIfEqualUserExists(existingUser.copy(id = 187))
      } yield {
        exists mustBe false
      }
    }

    "check if existing user exists (wrong name)" in { db =>
      import db._, db.ctx, ctx._
      for {
        Some(existingUser) <- db.user("heigo", "parwin")
        exists <- db.user.checkIfEqualUserExists(existingUser.copy(name = "heikola"))
      } yield {
        exists mustBe false
      }
    }

    "check if existing user exists (wrong isImplicit)" in { db =>
      import db._, db.ctx, ctx._
      for {
        Some(existingUser) <- db.user("heigo", "parwin")
        exists <- db.user.checkIfEqualUserExists(existingUser.copy(isImplicit = true))
      } yield {
        exists mustBe false
      }
    }

    "check if existing user exists (wrong revision)" in { db =>
      import db._, db.ctx, ctx._
      for {
        Some(existingUser) <- db.user("heigo", "parwin")
        exists <- db.user.checkIfEqualUserExists(existingUser.copy(revision = 3))
      } yield {
        exists mustBe false
      }
    }
  }

  "group" - {
    "create group for existing user" in { db =>
      import db._, db.ctx, ctx._
      for {
        Some(user) <- db.user("garna", "utria")
        Some((`user`, membership, group)) <- db.group.createForUser(user.id)
        queryGroups <- ctx.run(query[UserGroup])
        queryMemberships <- ctx.run(query[Membership])
      } yield {
        queryGroups must contain theSameElementsAs List(group)
        queryMemberships must contain theSameElementsAs List(membership)

        membership mustEqual Membership(user.id, group.id)
      }
    }

    "create group for non-existing user" in { db =>
      import db._, db.ctx, ctx._
      for {
        resultOpt <- db.group.createForUser(13153)
        queryGroups <- ctx.run(query[UserGroup])
        queryMemberships <- ctx.run(query[Membership])
      } yield {
        resultOpt mustEqual None
        queryGroups mustBe empty
        queryMemberships mustBe empty
      }
    }

    "add existing user to existing group" in { db =>
      import db._, db.ctx, ctx._
      for {
        Some(initialUser) <- db.user("garna", "utria")
        Some((_, _, group)) <- db.group.createForUser(initialUser.id)
        Some(user) <- db.user("furo", "garnaki")

        Some((_, membership, _)) <- db.group.addMember(group.id, user.id)
        queryMemberships <- ctx.run(query[Membership])
      } yield {
        membership mustEqual Membership(user.id, group.id)
        queryMemberships must contain theSameElementsAs List(Membership(initialUser.id, group.id), membership)
      }
    }

    "add existing user to existing group (is already member)" in { db =>
      import db._, db.ctx, ctx._
      for {
        Some(initialUser) <- db.user("garna", "utria")
        Some((_, _, group)) <- db.group.createForUser(initialUser.id)
        Some(user) <- db.user("furo", "garnaki")

        Some((_, _, _)) <- db.group.addMember(group.id, user.id)
        Some((_, membership, _)) <- db.group.addMember(group.id, user.id)
        queryMemberships <- ctx.run(query[Membership])
      } yield {
        membership mustEqual Membership(user.id, group.id)
        queryMemberships must contain theSameElementsAs List(Membership(initialUser.id, group.id), membership)
      }
    }

    "add non-existing user to existing group" in { db =>
      import db._, db.ctx, ctx._
      for {
        Some(initialUser) <- db.user("garna", "utria")
        Some((_, _, group)) <- db.group.createForUser(initialUser.id)
        Some(user) <- db.user("furo", "garnaki")

        membershipOpt <- db.group.addMember(group.id, 131551)
        queryMemberships <- ctx.run(query[Membership])
      } yield {
        membershipOpt mustEqual None
        queryMemberships must contain theSameElementsAs List(Membership(initialUser.id, group.id))
      }
    }

    "add existing user to non-existing group" in { db =>
      import db._, db.ctx, ctx._
      for {
        Some(user) <- db.user("garna", "utria")

        membershipOpt <- db.group.addMember(13515, user.id)
        queryMemberships <- ctx.run(query[Membership])
      } yield {
        membershipOpt mustEqual None
        queryMemberships mustBe empty
      }
    }

    "add non-existing user to non-existing group" in { db =>
      import db._, db.ctx, ctx._
      for {
        membershipOpt <- db.group.addMember(13515, 68415)
        queryMemberships <- ctx.run(query[Membership])
      } yield {
        membershipOpt mustEqual None
        queryMemberships mustBe empty
      }
    }

    "hasAccessToPost" - {
      "post in pubic group" in { db =>
        for {
          Some(user) <- db.user("u", "123456")
          (post, _) <- db.post("p", groupIdOpt = None)
          hasAccess <- db.group.hasAccessToPost(user.id, post.id)
        } yield hasAccess must be(true)
      }

      "post in private group (user not member)" in { db =>
        for {
          Some(user) <- db.user("u2", "123456")
          Some(user2) <- db.user("other", "123456")
          Some((_, _, group)) <- db.group.createForUser(user2.id)
          (post, _) <- db.post.createOwned("p", group.id)
          hasAccess <- db.group.hasAccessToPost(user.id, post.id)
        } yield hasAccess must be(false)
      }

      "post in private group (user is member)" in { db =>
        for {
          Some(user) <- db.user("u3", "123456")
          Some((_, _, group)) <- db.group.createForUser(user.id)
          (post, _) <- db.post.createOwned("p", group.id)
          hasAccess <- db.group.hasAccessToPost(user.id, post.id)
        } yield hasAccess must be(true)
      }
    }
  }

  "graph" - {
    "without user" - {
      "public posts, connections and containments" in { db =>
        import db._, db.ctx, ctx._
        for {
          postA <- db.post.createPublic("A")
          postB <- db.post.createPublic("B")
          postC <- db.post.createPublic("C")
          Some(conn) <- db.connection(postA.id, postB.id)
          Some(cont) <- db.containment(postB.id, postC.id)

          (posts, connections, containments,
            userGroups, ownerships, users, memberships) <- db.graph.getAllVisiblePosts(None)
        } yield {
          posts must contain theSameElementsAs List(postA, postB, postC)
          connections must contain theSameElementsAs List(conn)
          containments must contain theSameElementsAs List(cont)
          userGroups mustBe empty
          ownerships mustBe empty
          users mustBe empty
          memberships mustBe empty
        }
      }

      "public posts, connections and containments, private posts" in { db =>
        import db._, db.ctx, ctx._
        for {
          Some(user) <- db.user("heigo", "parwin")
          Some((_, membership, group)) <- db.group.createForUser(user.id)

          postA <- db.post.createPublic("A")
          (postB, ownershipB) <- db.post.createOwned("B", group.id)
          postC <- db.post.createPublic("C")
          Some(conn) <- db.connection(postA.id, postB.id)
          Some(cont) <- db.containment(postB.id, postC.id)

          (posts, connections, containments,
            userGroups, ownerships, users, memberships) <- db.graph.getAllVisiblePosts(None)
        } yield {
          posts must contain theSameElementsAs List(postA, postC)
          connections must contain theSameElementsAs List()
          containments must contain theSameElementsAs List()
          userGroups mustBe empty
          ownerships mustBe empty
          users mustBe empty
          memberships mustBe empty
        }
      }
    }

    "with user" - {
      "public posts, connections and containments" in { db =>
        import db._, db.ctx, ctx._
        for {
          Some(user) <- db.user("heigo", "parwin")
          postA <- db.post.createPublic("A")
          postB <- db.post.createPublic("B")
          postC <- db.post.createPublic("C")
          Some(conn) <- db.connection(postA.id, postB.id)
          Some(cont) <- db.containment(postB.id, postC.id)

          (posts, connections, containments,
            userGroups, ownerships, users, memberships) <- db.graph.getAllVisiblePosts(Option(user.id))
        } yield {
          posts must contain theSameElementsAs List(postA, postB, postC)
          connections must contain theSameElementsAs List(conn)
          containments must contain theSameElementsAs List(cont)
          userGroups mustBe empty
          ownerships mustBe empty
          users must contain theSameElementsAs List(user)
          memberships mustBe empty
        }
      }

      "group without posts" in { db =>
        import db._, db.ctx, ctx._
        for {
          Some(user) <- db.user("heigo", "parwin")
          Some((_, membership, group)) <- db.group.createForUser(user.id)

          (posts, connections, containments,
            userGroups, ownerships, users, memberships) <- db.graph.getAllVisiblePosts(Option(user.id))
        } yield {
          posts must contain theSameElementsAs List()
          connections must contain theSameElementsAs List()
          containments must contain theSameElementsAs List()
          userGroups mustEqual List(group)
          ownerships mustBe empty
          users must contain theSameElementsAs List(user)
          memberships must contain theSameElementsAs memberships
        }
      }

      "public posts, own private posts" in { db =>
        import db._, db.ctx, ctx._
        for {
          Some(user) <- db.user("heigo", "parwin")
          Some((_, membership, group)) <- db.group.createForUser(user.id)

          postA <- db.post.createPublic("A")
          (postB, ownershipB) <- db.post.createOwned("B", group.id)
          postC <- db.post.createPublic("C")
          Some(conn) <- db.connection(postA.id, postB.id)
          Some(cont) <- db.containment(postB.id, postC.id)

          (posts, connections, containments,
            userGroups, ownerships, users, memberships) <- db.graph.getAllVisiblePosts(Option(user.id))
        } yield {
          posts must contain theSameElementsAs List(postA, postB, postC)
          connections must contain theSameElementsAs List(conn)
          containments must contain theSameElementsAs List(cont)
          userGroups must contain theSameElementsAs List(group)
          ownerships must contain theSameElementsAs List(ownershipB)
          users must contain theSameElementsAs List(user)
          memberships must contain theSameElementsAs List(membership)
        }
      }

      "public posts, own private posts, invisible posts" in { db =>
        import db._, db.ctx, ctx._
        for {
          Some(user) <- db.user("heigo", "parwin")
          Some((_, membership, group)) <- db.group.createForUser(user.id)

          Some(otherUser) <- db.user("gurkulo", "meisin")
          Some((_, otherMembership, otherGroup)) <- db.group.createForUser(otherUser.id)

          postA <- db.post.createPublic("A")
          (postB, ownershipB) <- db.post.createOwned("B", group.id)
          (postC, ownershipC) <- db.post.createOwned("C", otherGroup.id)

          Some(conn) <- db.connection(postA.id, postB.id)
          Some(cont) <- db.containment(postB.id, postC.id)

          (posts, connections, containments,
            userGroups, ownerships, users, memberships) <- db.graph.getAllVisiblePosts(Option(user.id))
        } yield {
          posts must contain theSameElementsAs List(postA, postB)
          connections must contain theSameElementsAs List(conn)
          containments must contain theSameElementsAs List()
          userGroups must contain theSameElementsAs List(group)
          ownerships must contain theSameElementsAs List(ownershipB)
          users must contain theSameElementsAs List(user)
          memberships must contain theSameElementsAs List(membership)
        }
      }
    }
  }
}
