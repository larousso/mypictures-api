package controllers

import akka.actor.ActorSystem
import cats.{Functor, FunctorFilter}
import cats.data.OptionT
import play.api.{Configuration, Logger}
import play.api.libs.json._
import play.api.mvc._
import services.FacebookAuth
import services.account.Accounts
import services.account.Accounts.Role.Guest
import services.account.Accounts.{Role, _}
import services.validation.ValidatedT

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
    import cats.implicits._
    val jsResult: JsResult[LoginForm] = request.body.validate[LoginForm]
    val forbiddenMsg: Result = Forbidden(Json.obj("message" -> "Erreur de login ou de mot de passe"))
    (for {
      login <- ValidatedT.fromJsResult[Future](jsResult, errors => BadRequest(JsError.toJson(errors)))
      username <- ValidatedT.fromOption[Future](login.username, forbiddenMsg)
      password <- ValidatedT.fromOption[Future](login.password, forbiddenMsg)
      account <- ValidatedT.fromOptionF[Future](accounts.getAccountByUsername(username), forbiddenMsg)
    } yield if (password == account.password) {
        val session = Session(user = Some(SessionUser(account.username, account.role, Custom)))
        val sessionJson: JsValue = Json.toJson(session)
        Ok(sessionJson).withNewSession.withSession("user" -> Json.stringify(sessionJson))
      } else {
        forbiddenMsg
      }
    ).merge
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
