package com.adelegue.mypictures.domains

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Directives._
import akka.stream.{ActorMaterializer, Materializer}
import cats._
import com.adelegue.mypictures.domains.account.Accounts
import com.adelegue.mypictures.domains.account.Accounts.{DSL, RoleSerializer}
import com.softwaremill.session.SessionDirectives._
import com.softwaremill.session.SessionOptions._
import com.softwaremill.session.{JValueSessionSerializer, JwtSessionEncoder, SessionConfig, SessionManager}
import org.json4s.{DefaultFormats, jackson}

import scala.concurrent.{ExecutionContext, Future}

/**
  * Created by adelegue on 02/07/2016.
  */

object Auth {
  def apply(accountInterpreter: DSL ~> Future)(implicit ec: ExecutionContext, materializer: Materializer) = new Auth(accountInterpreter)(ec, materializer)
}

class Auth(accountInterpreter: Accounts.DSL ~> Future)(implicit ec: ExecutionContext, materializer: Materializer) {

  import Api._
  import cats.std.future._
  import de.heikoseeberger.akkahttpjson4s.Json4sSupport._
  import freek._
  implicit val serialization = jackson.Serialization
  implicit val formats = DefaultFormats + new SessionTypeSerializer + new RoleSerializer

  val sessionConfig = SessionConfig.default("some_very_long_secret_and_random_string_some_very_long_secret_and_random_string")
  implicit val serializer = JValueSessionSerializer.caseClass[Session]
  implicit val encoder = new JwtSessionEncoder[Session]
  implicit val manager = new SessionManager(sessionConfig)

  val facebookApi =
    pathPrefix("auth" / "facebook") {
      pathEnd {
        get {
          complete("")
        }
      } ~
        path("callback") {
          get {
            complete("")
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
              val session = Session(a.surname, a.role, Custom)
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
