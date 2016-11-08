package controllers

import java.util.UUID

import akka.actor.ActorSystem
import cats.data.Validated._
import play.api.libs.json.{JsError, Json}
import play.api.mvc.Controller
import services.comment.Comments

import scala.concurrent.{ExecutionContext, Future}

/**
  * Created by adelegue on 30/10/2016.
  */
class CommentsController(comments: Comments, actorSystem: ActorSystem)(implicit exec: ExecutionContext) extends Controller {


  def getAll(accountId: String, albumId: String, pictureId: String) = AuthAction.async {
    import Comments._
    comments.listCommentsByPicture(pictureId).map { c =>
      Ok(Json.toJson(c))
    }
  }

  def createComment(accountId: String, albumId: String, pictureId: String) =  AuthAction.async(parse.json) { request =>
    request.body.validate[Comments.Api.Comment](Comments.Api.format).fold(
      errors => Future.successful(BadRequest(Json.obj("status" -> "KO", "message" -> JsError.toJson(errors)))),
      {
        case Comments.Api.Comment(name, comment, date) =>
          comments.createComment(Comments.Comment(UUID.randomUUID.toString, pictureId, name, comment, date)).map {
            case Valid(c) => Created(Json.toJson(c.comment))
            case Invalid(e) => BadRequest
          }
      }
    )
  }

  def updateComment(accountId: String, albumId: String, pictureId: String, commentId: String) =  AuthAction.async(parse.json) { request =>
    request.body.validate[Comments.Comment](Comments.format).fold(
      errors => Future.successful(BadRequest(Json.obj("status" -> "KO", "message" -> JsError.toJson(errors)))),
      c => comments.updateComment(c).map {
          case Valid(c) => Created(Json.toJson(c.comment))
          case Invalid(e) => BadRequest
        }
    )
  }

  def readComments(accountId: String, albumId: String, pictureId: String, commentId: String) = AuthAction.async {
    import Comments._
    comments.getComment(commentId).map {
      case Some(c) => Ok(Json.toJson(c))
      case None => NotFound
    }
  }

  def deleteComments(accountId: String, albumId: String, pictureId: String, commentId: String) = AuthAction.async {
    comments.deleteComment(commentId).map { _ =>
      NoContent
    }
  }

}
