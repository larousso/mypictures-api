package controllers

import akka.actor.ActorSystem
import play.api.{Configuration, Logger}
import play.api.libs.json._
import play.api.mvc.{Action, BodyParsers, Controller, Request}
import services.FacebookAuth
import services.account.Accounts
import services.account.Accounts.Role.Guest
import services.account.Accounts.{Role, _}

import scala.concurrent.{ExecutionContext, Future}

/**
  * Created by adelegue on 30/10/2016.
  */
class SessionController(accounts: Accounts, facebookAuth: FacebookAuth, config: Configuration, actorSystem: ActorSystem)(implicit exec: ExecutionContext) extends Controller {

  val defaultRedirect = s"http://${config.getString("app.host").get}:${config.getInt("app.port").get}/api/session"


  def getSession() = Action { request =>
      Logger.debug(s"${request.session} - ${request.cookies}")
      request.session.get("user").map { u =>
        Ok((Json.parse(u) \ "user").as[JsObject])
      } getOrElse NotFound
  }

  def doLogin() = Action.async(BodyParsers.parse.json) { request =>
    request.body.validate[LoginForm].fold(
      errors => Future.successful(BadRequest(Json.obj("status" -> "KO", "message" -> JsError.toJson(errors)))),
      login => {
        val mayBeLogin = for {
          username <- login.username
          password <- login.password
        } yield {
          accounts.getAccountByUsername(username).map {
            case Some(a) if password == a.password =>
              val session = Session(user = Some(SessionUser(a.username, a.role, Custom)))
              val sessionJson: JsValue = Json.toJson(session)
              Ok(sessionJson).withNewSession.withSession("user" -> Json.stringify(sessionJson))
            case _ => Forbidden(Json.obj("message" -> "Erreur de login ou de mot de passe"))
          }
        }
        mayBeLogin getOrElse Future.successful(Forbidden(Json.obj("message" -> "Erreur de login ou de mot de passe")))
      })
  }

  def facebookRedirect(redirection: Option[String]) = Action {
    val session = Session(user= None, redirect = redirection)
    val sessionJson: JsValue = Json.toJson(session)
    SeeOther(facebookAuth.auth).withSession("user" -> Json.stringify(sessionJson))
  }

  def facebookCallback(code: String) = Action.async { request =>
    for {
      token <- facebookAuth.accessToken(code).map { json => (json \ "access_token").as[String] }
      fbName <- facebookAuth.me(token).map { json => (json \ "name").as[String] }
    } yield Session.parseSession(request) match {
          case Some(s) =>
            val red = s.redirect.getOrElse(defaultRedirect)
            val session = Session(user = Some(SessionUser(fbName, Guest, Facebook)))
            val sessionJson: JsValue = Json.toJson(session)
            SeeOther(red).withSession("user" -> Json.stringify(sessionJson))
          case None =>
            val session = Session(user = Some(SessionUser(fbName, Guest, Facebook)))
            val sessionJson: JsValue = Json.toJson(session)
            SeeOther(defaultRedirect).withSession("user" -> Json.stringify(sessionJson))
        }
  }

}
