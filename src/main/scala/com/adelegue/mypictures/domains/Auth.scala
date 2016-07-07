package com.adelegue.mypictures.domains

import akka.actor.ActorSystem
import akka.http.scaladsl.server.AuthorizationFailedRejection
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.{Directive0, Route}
import akka.stream.Materializer
import cats._
import com.adelegue.mypictures.domains.account.Accounts
import com.adelegue.mypictures.domains.account.Accounts.Role.Guest
import com.adelegue.mypictures.domains.account.Accounts.{DSL, Role, RoleSerializer}
import com.softwaremill.session.SessionDirectives._
import com.softwaremill.session.SessionOptions._
import com.softwaremill.session.{JValueSessionSerializer, JwtSessionEncoder, SessionConfig, SessionManager}
import com.typesafe.config.Config
import org.json4s.{DefaultFormats, jackson}

import scala.concurrent.Future

/**
  * Created by adelegue on 02/07/2016.
  */

object Auth {
  def apply(config: Config, accountInterpreter: DSL ~> Future)(implicit system: ActorSystem, materializer: Materializer) = new Auth(config, accountInterpreter)(system, materializer)
}

class Auth(config: Config, accountInterpreter: Accounts.DSL ~> Future)(implicit system: ActorSystem, materializer: Materializer) {

  import Api._
  import cats.std.future._
  import de.heikoseeberger.akkahttpjson4s.Json4sSupport._
  import freek._
  import system.dispatcher
  implicit val serialization = jackson.Serialization
  implicit val formats = DefaultFormats + new SessionTypeSerializer + new RoleSerializer

  val sessionConfig = SessionConfig.default("some_very_long_secret_and_random_string_some_very_long_secret_and_random_string")
  implicit val serializer = JValueSessionSerializer.caseClass[Session]
  implicit val encoder = new JwtSessionEncoder[Session]
  implicit val manager = new SessionManager(sessionConfig)

  val fAuth = new FacebookAuth(config.getString("facebook.appId"), config.getString("facebook.appSecret"))
  val defaultRedirect = s"http://${config.getString("app.host")}:${config.getInt("app.port")}/api/session"

  def hashRole[Unit](role: Role): Directive0 = {
    optionalSession(oneOff, usingCookies).flatMap {
      case Some(Session(Some(user), _)) if user.role == role =>
        pass
      case None =>
        reject(AuthorizationFailedRejection)
    }
  }

  val facebookApi =
    pathPrefix("auth" / "facebook") {
      pathEnd {
        get {
          parameters('redirect.?) { redirection =>
            val session = Session(user= None, redirect = redirection)
            setSession(oneOff, usingCookies, session) {
              redirect(fAuth.auth, StatusCodes.SeeOther)
            }
          }
        }
      } ~
        path("callback") {
          get {
            parameters('code) { code =>
              onSuccess(fAuth
                .accessToken(code).map{ json => (json \ "access_token").extract[String]}
                .flatMap(fAuth.me)) { json =>
                    optionalSession(oneOff, usingCookies) {
                      case Some(session) =>
                        val red = session.redirect.getOrElse(defaultRedirect)
                        setSession(oneOff, usingCookies, Session(user= Some(SessionUser((json \ "name").extract[String], Guest, Facebook)))) {
                          redirect(red, StatusCodes.SeeOther)
                        }
                      case None =>
                        setSession(oneOff, usingCookies, Session(user= Some(SessionUser((json \ "name").extract[String], Guest, Facebook)))) {
                          redirect(defaultRedirect, StatusCodes.SeeOther)
                        }
                    }
              }
            }
          }
        }
    }

  val loginApi =
    path("session") {
      get {
        optionalSession(oneOff, usingCookies) {
          case Some(session) =>
            complete(session)
          case None =>
            complete(StatusCodes.NotFound)
        }
      }
    } ~
    path("login") {
      post {
        entity(as[LoginForm]) { login =>
          println(s"User $login")
          complete(login)
          onSuccess(Accounts.getAccountByUsername(login.login).interpret(Interpreter(accountInterpreter))) {
            case Some(a) if a.password == login.password =>
              println(s"Found !")
              val session = Session(user= Some(SessionUser(a.surname, a.role, Custom)))
              setSession(oneOff, usingCookies, session) {
                complete(session)
              }
            case _ =>
              println("Unauthorized")
              complete(StatusCodes.Unauthorized)
          }
        }
      }
    }

}
