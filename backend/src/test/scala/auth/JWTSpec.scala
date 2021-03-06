package wust.backend.auth

import org.scalatest._
import wust.graph.User
import wust.ids._
import java.time.Duration

class JWTSpec extends FreeSpec with MustMatchers {
  val jwt = new JWT("secret", Duration.ofHours(1))

  implicit def intToUserId(id: Int): UserId = UserId(id)

  object User {
    def apply(name: String): User = new User(1313L, name, isImplicit = false, 0)
  }

  "generate auth and then to authentication" in {
    val user = User("Biermann")
    val auth = jwt.generateAuthentication(user)
    val clientAuth = auth.toAuthentication

    clientAuth.user mustEqual auth.user
    clientAuth.token mustEqual auth.token
  }

  "generate auth for user" in {
    val user = User("Biermann")
    val auth = jwt.generateAuthentication(user)

    auth.user mustEqual user
    auth.expires must be > (System.currentTimeMillis / 1000)
    auth.token.length must be > 0
  }

  "generated auth is not expired" in {
    val user = User("Frau Mahlzahn")
    val auth = jwt.generateAuthentication(user)

    jwt.isExpired(auth) mustEqual false
  }

  "expired auth is expired" in {
    val user = User("Frau Mahlzahn")
    val jwt = new JWT("secret", tokenLifetime = Duration.ZERO)
    val auth = jwt.generateAuthentication(user)
    jwt.isExpired(auth) mustEqual true
  }

  "incompatible secret not valid" in {
    val user = User("Frau Mahlzahn")

    val jwt1 = new JWT("gisela", tokenLifetime = Duration.ofHours(1))
    val auth = jwt1.generateAuthentication(user)

    val jwt2 = new JWT("hans", tokenLifetime = Duration.ofHours(1))
    val noAuth = jwt2.authenticationFromToken(auth.token)

    noAuth mustEqual None
  }

  "authentication from token" in {
    val user = User("Pumuckl")
    val genAuth = jwt.generateAuthentication(user)
    val auth = jwt.authenticationFromToken(genAuth.token)

    auth mustEqual Option(genAuth)
  }

  "no authentication from invalid token" in {
    val auth = jwt.authenticationFromToken("invalid token")

    auth mustEqual None
  }
}
