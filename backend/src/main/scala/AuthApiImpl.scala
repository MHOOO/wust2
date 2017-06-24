package wust.backend

import com.roundeights.hasher.Hasher
import wust.api._
import wust.backend.DbConversions._
import wust.backend.auth._
import wust.framework.state._
import wust.db.Db
import wust.util.RichFuture

import scala.concurrent.{ExecutionContext, Future}

class AuthApiImpl(holder: StateHolder[State, ApiEvent], dsl: GuardDsl, db: Db)(implicit ec: ExecutionContext) extends AuthApi {
  import holder._, dsl._

  private def passwordDigest(password: String) = Hasher(password).bcrypt

  //==== converting implicit-real users:
  //TODO: propagate new groups into state?
  //TODO: propagate name change to the respective groups and the connected clients

  def register(name: String, password: String): Future[Boolean] = { (state: State) =>
    val digest = passwordDigest(password)
    state.auth.map(_.user) match {
      case Some(user) if user.isImplicit =>
        db.user.activateImplicitUser(user.id, name, digest).map(_.map(u => JWT.generateAuthentication(u))).map {
          case None => respondWithEvents(false)
          case Some(auth) => respondWithEvents(true, LoggedIn(auth.toAuthentication))
        }
      case currentAuth =>
        db.user(name, digest).map(_.map(u => JWT.generateAuthentication(u))).map {
          case None if currentAuth.isDefined => respondWithEvents(false, LoggedOut)
          case None => respondWithEvents(false)
          case Some(auth) => respondWithEvents(true, LoggedIn(auth.toAuthentication))
        }
    }
  }

  def login(name: String, password: String): Future[Boolean] = { (state: State) =>
    val digest = passwordDigest(password)
    db.user.getUserAndDigest(name).map {
      case Some((user, userDigest)) if (digest hash = userDigest) =>
        //TODO integrate result into response?
        state.auth.foreach { auth =>
          if (auth.user.isImplicit)
            db.user.mergeImplicitUser(auth.user.id, user.id).log
        }

        val auth = JWT.generateAuthentication(user)
        respondWithEvents(true, LoggedIn(auth.toAuthentication))
      case _ => state.auth match {
        case None => respondWithEvents(false)
        case Some(auth) if auth.user.isImplicit => respondWithEvents(false)
        case Some(_) => respondWithEvents(false, LoggedOut)
      }
    }
  }

  //TODO: implicit handling?
  def loginToken(token: Authentication.Token): Future[Boolean] = { (state: State) =>
    val auth = JWT.authenticationFromToken(token).map { auth =>
      for (valid <- db.user.checkIfEqualUserExists(auth.user))
        yield if (valid) Option(auth) else None
    }.getOrElse(Future.successful(None))

    auth map {
      case None => respondWithEvents(false, LoggedOut)
      case Some(auth) => respondWithEvents(true, LoggedIn(auth.toAuthentication))
    }
  }

  def logout(): Future[Boolean] = { (state: State) =>
    Future.successful(respondWithEvents(true, LoggedOut))
  }
}
