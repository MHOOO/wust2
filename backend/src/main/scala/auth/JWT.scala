package wust.backend.auth

import derive.derive
import pdi.jwt.{JwtCirce, JwtAlgorithm, JwtClaim}
import wust.api._
import wust.backend.config.Config
import wust.graph.User
import wust.ids._
import scala.util.{Success, Failure}
import java.time.Instant
import java.time.Duration

@derive((user, expires) => toString)
case class JWTAuthentication private[auth] (user: User, expires: Long, token: Authentication.Token) {
  def toAuthentication = Authentication(user, token)
}

class JWT(secret: String, tokenLifetime: Duration) {
  import io.circe._, io.circe.syntax._, io.circe.generic.semiauto._
  implicit val userDecoder: Decoder[User] = deriveDecoder[User]
  implicit val userEncoder: Encoder[User] = deriveEncoder[User]

  private val algorithm = JwtAlgorithm.HS256
  private val issuer = "wust"
  private val audience = "wust"

  def generateClaim(user: User, expires: Long) = {
    JwtClaim(content = user.asJson.toString)
      .by(issuer)
      .to(audience)
      .startsNow
      .issuedNow
      .expiresAt(expires)
  }

  def generateAuthentication(user: User): JWTAuthentication = {
    val expires = Instant.now.getEpochSecond + tokenLifetime.getSeconds
    val claim = generateClaim(user, expires)
    val token = JwtCirce.encode(claim, secret, algorithm)
    JWTAuthentication(user, expires, token)
  }

  def authenticationFromToken(token: Authentication.Token): Option[JWTAuthentication] = {
    JwtCirce.decode(token, secret, Seq(algorithm)).toOption.collect {
      case claim if claim.isValid(issuer, audience) => for {
        expires <- claim.expiration
        user <- parser.decode[User](claim.content).right.toOption
      } yield JWTAuthentication(user, expires, token)
    }.flatten
  }

  def isExpired(auth: JWTAuthentication): Boolean = auth.expires <= Instant.now.getEpochSecond
}
object JWT extends JWT(Config.auth.secret, Config.auth.tokenLifetime)
