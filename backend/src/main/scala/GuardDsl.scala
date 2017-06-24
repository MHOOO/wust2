package wust.backend

import wust.api._
import wust.db.Db
import wust.framework.state._
import wust.backend.auth._
import wust.graph._
import DbConversions._

import scala.concurrent.{ ExecutionContext, Future }

class GuardDsl(db: Db, stateInterpreter: StateInterpreter, enableImplicit: Boolean)(implicit ec: ExecutionContext) {
  private def createImplicitAuth() = enableImplicit match {
    case true => db.user.createImplicitUser().map(u => JWT.generateAuthentication(forClient(u))).map(Option.apply)
    case false => Future.successful(None)
  }

  private lazy val implicitAuth = createImplicitAuth()

  def withUser[T](f: (State, User) => Future[RequestResponse[T, ApiEvent]]): State => Future[RequestResponse[T, ApiEvent]] = state => {
    val user = state.auth.map(_.user).getOrElse(throw ApiException(Unauthorized))
    f(state, user)
  }

  def withUserOrImplicit[T](f: (State, User) => Future[RequestResponse[T, ApiEvent]]): State => Future[RequestResponse[T, ApiEvent]] = state => {
    def getUser(state: State): Future[RequestResponse[User, ApiEvent]] = state.auth match {
      case None => implicitAuth map {
        case Some(auth) => RequestResponse[User, ApiEvent](auth.user, Seq(LoggedIn(auth.toAuthentication)))
        case None => throw new ApiException(Unauthorized)
      }
      case Some(auth) => Future.successful(RequestResponse[User, ApiEvent](auth.user))
    }

    combine[User, T](getUser _, f)(state)
  }

  def combine[IN,OUT](f: State => Future[RequestResponse[IN, ApiEvent]], h: (State, IN) => Future[RequestResponse[OUT, ApiEvent]]): State => Future[RequestResponse[OUT, ApiEvent]] = state => {
    f(state).flatMap { response =>
      val result = response.result
      //TODO this is done twice now, once here. and again when the whole function is evaluated in a request => Server.scala
      // the new state here is not visible to anyone outside of the h function
      stateInterpreter.applyEventsToState(state, response.events).flatMap { newState =>
        h(newState, result).map(r => r.copy(events = response.events ++ r.events))
      }
    }
  }
}
