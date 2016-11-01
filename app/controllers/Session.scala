package controllers

import play.api.libs.json._
import play.api.mvc.Request
import services.account.Accounts.{Role, Username}

/**
  * Created by adelegue on 31/10/2016.
  */


object SessionType {

  implicit val sessionTypeFormat = Format(
    Reads[SessionType] {
      case JsString(s) if s == Facebook.sessionType =>
      JsSuccess(Facebook)
      case JsString(s) if s == Custom.sessionType =>
      JsSuccess(Custom)
      case _ => JsError("wrong session type")
    }, Writes[SessionType] { t =>
      JsString(t.sessionType)
    }
  )
}

sealed trait SessionType {
  def sessionType: String
}

case object Facebook extends SessionType {
  override val sessionType = "facebook"
}

case object Custom extends SessionType {
  override val sessionType = "custom"
}

case class LoginForm(username: Option[String], password: Option[String]) extends Serializable

object LoginForm {
  implicit val format = Json.format[LoginForm]
}

case class SessionUser(username: Username, role: Role, sessionType: SessionType)

object SessionUser {
  implicit val format = Json.format[SessionUser]
}

case class Session(user: Option[SessionUser], redirect: Option[String] = None)

object Session {
  import SessionType._
  import SessionUser._
  implicit val sessionFormat = Json.format[Session]

  def parseSession(request: Request[_]): Option[Session] =
    request.session.get("user")
      .map(Json.parse)
      .map(sessionFormat.reads)
      .flatMap(jsResult => jsResult.asOpt)

}
