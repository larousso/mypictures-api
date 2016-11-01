package controllers

import java.util.{Date, UUID}

import akka.actor.ActorSystem
import play.api.libs.json.{JsError, Json}
import play.api.mvc.{BodyParsers, Controller}
import services.account.Accounts.Role.Admin
import services.album.Albums
import services.picture.Pictures

import scala.concurrent.{ExecutionContext, Future}
import scalaz.{Failure, Success}

/**
  * Created by adelegue on 30/10/2016.
  */
class AlbumsController(albums: Albums, pictures: Pictures, actorSystem: ActorSystem)(implicit exec: ExecutionContext) extends Controller {

  object Api {
    case class Album(title: Albums.Title, description: Option[Albums.Description], date: Date = new Date(), pictureIds: List[Pictures.Id] = List.empty[Pictures.Id])
    implicit val format = Json.format[Album]
  }

  def getAll(accountId: String) = AuthAction.async {
    import Albums._
    albums.getAlbumByUsername(accountId).map { a =>
      Ok(Json.toJson(a))
    }
  }

  def create(accountId: String) = AuthAction(Admin).async(BodyParsers.parse.json) { request =>
    request.body.validate[Api.Album](Api.format).fold(
      errors => Future.successful(BadRequest(Json.obj("status" ->"KO", "message" -> JsError.toJson(errors)))),
      {
        case Api.Album(title, description, date, pictureId) =>
          albums.createAlbum(Albums.Album(UUID.randomUUID.toString, accountId, title, description, date, pictureId)).map {
            case Success(a) => Created(Json.toJson(a.album)(Albums.format))
            case Failure(e) => BadRequest
          }
      }
    )
  }

  def updateAlbum(accountId: String, albumId: String) = update()

  def update() = AuthAction(Admin).async(BodyParsers.parse.json) { request =>
    request.body.validate[Albums.Album](Albums.format).fold(
      errors => Future.successful(BadRequest(Json.obj("status" ->"KO", "message" -> JsError.toJson(errors)))),
      album =>
          albums.updateAlbum(album).map {
            case Success(a) => Ok(Json.toJson(a.album)(Albums.format))
            case Failure(e) => BadRequest
          }
    )
  }

  def getById(accountId: String, albumId: String) = get(albumId)

  def get(albumId: String) = AuthAction.async {
    import Albums._
    albums.getAlbum(albumId).map {
      case Some(a) => Ok(Json.toJson(a))
      case None => NotFound
    }
  }


  def deleteById(accountId: String, albumId: String) = delete(albumId)

  def delete(albumId: String) = AuthAction(Admin).async {
    val res = for {
      _ <- pictures.deletePicturesByAlbum(albumId)
      delete <- albums.deleteAlbum(albumId)
    } yield delete

    res.map {
      case Success(a) =>
        Ok(Json.obj("id" -> a.id))
      case Failure(e) =>
        BadRequest
    }
  }



}
