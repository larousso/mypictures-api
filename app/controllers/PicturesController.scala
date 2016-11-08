package controllers

import akka.actor.ActorSystem
import akka.stream.scaladsl.Sink
import akka.util.ByteString
import play.api.libs.json.{JsError, JsResult, Json}
import play.api.libs.streams.Accumulator
import play.api.mvc.MultipartFormData.FilePart
import play.api.mvc.{Action, BodyParsers, Controller}
import play.core.parsers.Multipart.FileInfo
import services.account.Accounts.Role.Admin
import services.picture.Pictures

import scala.concurrent.{ExecutionContext, Future}
import cats.data.Validated._
import services.picture.Pictures.Api.RotationAction
import services.validation.ValidatedT

/**
  * Created by adelegue on 30/10/2016.
  */
class PicturesController(pictures: Pictures, actorSystem: ActorSystem)(implicit exec: ExecutionContext) extends Controller {


  def readThumbnail(id: String) = Action.async {
    pictures.readThumbnail(id).map {
      case Some(bytes) => Ok(bytes).as("image/jpeg")
      case None => NotFound
    }
  }

  def readImage(id: String) = Action.async {
    pictures.readImage(id).map {
      case Some(bytes) => Ok(bytes).as("image/jpeg")
      case None => NotFound
    }
  }

  def getAll(accountId: String, albumId: String) = getPicturesByAlbumId(albumId)

  def getPicturesByAlbumId(albumId: String) = AuthAction.async {
    import Pictures._
    pictures.getPictureByAlbum(albumId).map { p =>
      Ok(Json.toJson(p))
    }
  }

  def getById(accountId: String, albumId: String, pictureId: String) = get(pictureId)

  def getByIdAndAlbum(albumId: String, pictureId: String) = get(pictureId)

  def get(pictureId: String) = Action.async {
    import Pictures._
    pictures.getPicture(pictureId).map {
      case Some(p) => Ok(Json.toJson(p))
      case None => NotFound
    }
  }

  def deletePicture(accountId: String, albumId: String, pictureId: String) = delete(albumId, pictureId)

  def delete(albumId: String, pictureId: String) = AuthAction.async {
    pictures.deletePicture(albumId, pictureId).map { _ => NoContent }
  }

  type FilePartHandler[A] = FileInfo => Accumulator[ByteString, FilePart[A]]

  def handleFilePartAsFile: FilePartHandler[Array[Byte]] = {
    case FileInfo(partName, filename, contentType) =>
      val sink: Sink[ByteString, Future[ByteString]] = Sink.fold(ByteString.empty)(_ ++ _)
      val accumulator : Accumulator[ByteString, ByteString] = Accumulator(sink)
      accumulator.map {
        case b: ByteString =>
          FilePart(partName, filename, contentType, b.toArray)
      }(play.api.libs.concurrent.Execution.defaultContext)
  }

  def createPicture(accountId: String, albumId: String, pictureId: String) = create(albumId, pictureId)

  def create(albumId: String, pictureId: String) = AuthAction(Admin).async(parse.multipartFormData(handleFilePartAsFile)) { request =>
    request.body.file("file").map {
      case FilePart(key, filename, contentType, file) =>
        val imgType: Pictures.Type = "image/jpeg"
        val picture = Pictures.Picture(pictureId, filename, imgType, albumId)
        pictures.createPicture(picture, file).map {
          case Valid(p) => Created(Json.toJson(p.picture))
          case Invalid(e) => BadRequest
        }
    }.getOrElse(
      Future.successful(BadRequest)
    )
  }

  def updatePicture(accountId: String, albumId: String, pictureId: String) = update(albumId, pictureId)

  def update(albumId: String, pictureId: String) = AuthAction(Admin).async(parse.json) { request =>
    import Pictures._
    import cats.implicits._
    val jsResult: JsResult[Picture] = request.body.validate[Picture]
    (for {
      picture <- ValidatedT.fromJsResult[Future](jsResult, errors => BadRequest(JsError.toJson(errors)))
      updated <- ValidatedT(pictures.updatePicture(picture)).invalidMap(e => BadRequest(""))
    } yield Ok(Json.toJson(updated.picture))).merge
  }

  def rotatePicture(accountId: String, albumId: String, pictureId: String) = rotate(albumId, pictureId)

  def rotate(albumId: String, pictureId: String) = AuthAction(Admin).async(BodyParsers.parse.json) { request =>
    import Pictures._
    import cats.implicits._
    val jsResult: JsResult[RotationAction] = request.body.validate[RotationAction](Api.format)
    (for {
      action <- ValidatedT.fromJsResult[Future](jsResult, errors => BadRequest(JsError.toJson(errors)))
      updated <- ValidatedT(pictures.rotatePicture(pictureId, action.rotation)).invalidMap(e => BadRequest(""))
    } yield Ok(Json.toJson(updated))).merge
  }

}
