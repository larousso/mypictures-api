package controllers

import akka.actor.ActorSystem
import play.api.libs.json._
import play.api.mvc._
import play.api.{Configuration, Logger}
import services.FacebookAuth
import services.account.Accounts
import services.account.Accounts.Role.Guest
import services.account.Accounts._
import services.validation.ValidatedT

import scala.concurrent.{ExecutionContext, Future}

/**
  * Created by adelegue on 30/10/2016.
  */
class SessionController(accounts: Accounts, facebookAuth: FacebookAuth, config: Configuration, actorSystem: ActorSystem)(implicit exec: ExecutionContext) extends Controller {


  private val port = config.getInt("app.client.port").map(p => s":$p").getOrElse("")
  val defaultRedirect = s"http://${config.getString("app.host").get}$port"


  def getSession() = Action { request =>
      Logger.debug(s"Session: ${request.session} - Cookies: ${request.cookies}")
      Session.parseSession(request).map { s =>
        Logger.debug(s"user: $s")
        Ok(Json.toJson(s.user))
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
        Ok(Json.toJson(session)).withNewSession.withSession("user" -> session.stringify)
      } else {
        forbiddenMsg
      }
    ).merge
  }

  def facebookRedirect(redirection: Option[String]) = Action {
    val session = Session(user= None, redirect = redirection)
    SeeOther(facebookAuth.auth).withSession("user" -> session.stringify)
  }

  def facebookCallback(code: String) = Action.async { request =>
    for {
      token <- facebookAuth.accessToken(code).map { json => (json \ "access_token").as[String] }
      fbName <- facebookAuth.me(token).map { json => (json \ "name").as[String] }
    } yield Session.parseSession(request) match {
      case Some(s) =>
        val red = getRedirect(s.redirect)
        val session = Session(user = Some(SessionUser(fbName, Guest, Facebook)))
        SeeOther(red).withSession("user" -> session.stringify)
      case None =>
        val session = Session(user = Some(SessionUser(fbName, Guest, Facebook)))
        SeeOther(defaultRedirect).withSession("user" -> session.stringify)
    }
  }

  def getRedirect(url: Option[String]): String = {
    url match {
      case Some(r) if r.startsWith("http") => r
      case Some(r) if r.startsWith("/") => s"$defaultRedirect$r"
      case Some(r) => s"$defaultRedirect/$r"
      case _ => defaultRedirect
    }
  }


}
