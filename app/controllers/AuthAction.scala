package controllers

import play.api.mvc._
import services.account.Accounts.Role
import services.account.Accounts.Role.Admin

import scala.concurrent.Future

/**
  * Created by adelegue on 31/10/2016.
  */
object AuthAction extends AuthActionM {def role = None}

case class AuthAction(r: Role) extends AuthActionM { def role = Some(r) }

trait AuthActionM {

  def role: Option[Role]

  def async(block: => Future[Result]): Action[AnyContent] = {
    Action.async { applyAuth[AnyContent](role)(_ => block) }
  }

  def async[T](bodyParser: BodyParser[T])(block: Request[T] => Future[Result]): Action[T] = {
    Action.async(bodyParser) { applyAuth[T](role)(block) }
  }

  private def applyAuth[T](role: Option[Role])(block: (Request[T]) => Future[Result]): (Request[T]) => Future[Result] = {
    request =>
      val maybeResult: Option[Future[Result]] = for {
        session <- Session.parseSession(request)
        user <- session.user
      } yield user.role match {
        case Admin => block(request)
        case anyRole if role.isEmpty => block(request)
        case anyRole if role.contains(anyRole) => block(request)
        case _ => Future.successful(Results.Forbidden)
      }
      maybeResult getOrElse Future.successful(Results.Forbidden)
  }

}