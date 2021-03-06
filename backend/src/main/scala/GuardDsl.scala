package wust.backend

import wust.api._
import wust.db.Db
import wust.framework.state._
import wust.backend.auth._
import wust.graph._
import DbConversions._

import scala.concurrent.{ ExecutionContext, Future }

class GuardDsl(db: Db, enableImplicit: Boolean)(implicit ec: ExecutionContext) {
  private def createImplicitAuth() = enableImplicit match {
    case true => db.user.createImplicitUser().map(u => JWT.generateAuthentication(forClient(u))).map(Option.apply)
    case false => Future.successful(None)
  }

  private lazy val implicitAuth = createImplicitAuth()

  private def actualOrImplicitAuth(auth: Option[JWTAuthentication]): Future[Option[JWTAuthentication]] = auth match {
    case None => implicitAuth
    case auth => Future.successful(auth)
  }

  private def userOrFail(auth: Option[JWTAuthentication]): User =
    auth.map(_.user).getOrElse(throw ApiException(Unauthorized))

  def withUser[T](f: (State, User) => Future[RequestResponse[T, ApiEvent]]): State => Future[RequestResponse[T, ApiEvent]] = state => {
    val user = userOrFail(state.auth)
    f(state, user)
  }

  def withUserOrImplicit[T](f: (State, User) => Future[RequestResponse[T, ApiEvent]]): State => StateEffect[State, T, ApiEvent] = state => {
    val auth = actualOrImplicitAuth(state.auth)
    val newState = auth.map(auth => state.copy(auth = auth))
    val user = auth.map(userOrFail _)
    val response = newState.flatMap(newState => user.flatMap(f(newState, _)))
    StateEffect(newState, response)
  }
}
