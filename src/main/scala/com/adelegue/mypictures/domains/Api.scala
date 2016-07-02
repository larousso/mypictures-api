package com.adelegue.mypictures.domains

import akka.actor.ActorSystem
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import akka.stream.{ActorMaterializer, Materializer}
import com.adelegue.mypictures.domains.account.Accounts
import com.adelegue.mypictures.domains.album.Albums
import com.adelegue.mypictures.domains.account.impl.AccountInterpreter
import com.adelegue.mypictures.domains.album.impl.AlbumInterpreter
import com.adelegue.mypictures.domains.picture.Images
import com.adelegue.mypictures.domains.picture.Pictures
import com.adelegue.mypictures.domains.picture.impl.{ImagesInterpreter, PicturesInterpreter}
import org.json4s.DefaultFormats
import org.json4s.native.Serialization

import scala.concurrent.ExecutionContext
import scalaz.{Failure, Success}

/**
  * Created by adelegue on 30/06/2016.
  */
object Api {
  def apply()(implicit system: ActorSystem, materializer: ActorMaterializer): Api = new Api
}

class Api(implicit system: ActorSystem, materializer: ActorMaterializer) {

  import de.heikoseeberger.akkahttpjson4s.Json4sSupport._
  implicit val serialization = Serialization
  implicit val formats = DefaultFormats

  import freek._
  implicit val ec: ExecutionContext = system.dispatcher
  import cats.std.future._

  val accountInterpreter = AccountInterpreter(system)
  val albumInterpreter = AlbumInterpreter(system) :&: accountInterpreter
  val pictureInterpreter = PicturesInterpreter(system) :&: ImagesInterpreter("") :&: albumInterpreter

  case class Action(rotation: Images.Rotation)

  val route: Route =
    pathPrefix( "accounts" / "\\w+".r ) { username: Accounts.Username =>
      pathEnd {
        get {
            onSuccess(Accounts.getAccountByUsername(username).interpret(Interpreter(accountInterpreter))) {
                case Some(a) => complete(a)
                case None => complete(StatusCodes.NotFound)
            }
        }
      } ~
      path( "albums" ) {
        pathEnd {
          get {
            onSuccess(Albums.getAlbumByUsername(username).interpret(albumInterpreter)) { albums =>
              complete(albums)
            }
          } ~
          post {
            decodeRequest {
              entity(as[Albums.Album]) { album =>
                onSuccess(Albums.createAlbum(album).interpret(albumInterpreter)) {
                  case Success(_) => complete(StatusCodes.Created)
                  case Failure(e) => complete(StatusCodes.BadRequest, e)
                }
              }
            }
          }
        } ~
        path("\\w+".r) { albumId: Albums.Id =>
          pathEnd {
            get {
              onSuccess(Albums.getAlbum(albumId).interpret(albumInterpreter)) {
                case Some(a) => complete(a)
                case None => complete(StatusCodes.NotFound)
              }
            } ~
            delete {
              onSuccess(Albums.deleteAlbum(albumId).interpret(albumInterpreter)) { _ =>
                complete(StatusCodes.NoContent)
              }
            } ~
            put {
              decodeRequest {
                entity(as[Albums.Album]) { album =>
                  onSuccess(Albums.updateAlbum(album).interpret(albumInterpreter)) {
                    case Success(_) => complete(StatusCodes.Created)
                    case Failure(e) => complete(StatusCodes.BadRequest, e)
                  }
                }
              }
            }
          } ~
          path("pictures") {
            pathEnd {
              get {
                onSuccess(Pictures.getPictureByAlbum(albumId).interpret(pictureInterpreter)) { pictures =>
                  complete(pictures)
                }
              }
            } ~
            path("\\w+".r) { pictureId: Pictures.Id =>
              pathEnd {
                post {
                  fileUpload("file") {
                    case (metadata, byteSource) =>
                      val filename: Pictures.Filename = metadata.fileName
                      metadata.contentType.mediaType.fileExtensions
                      val extension: Pictures.Extension = "jpg"
                      val picture = Pictures.Picture(pictureId, filename, extension, albumId)
                      onSuccess(byteSource
                        .map(_.utf8String)
                        .runFold("") { (acc, e) => acc ++ e }
                        .flatMap { fileContent =>
                          Pictures.createPicture(picture, fileContent.getBytes).interpret(pictureInterpreter)
                        }) {
                        case Success(_) => complete(StatusCodes.Created)
                        case Failure(e) => complete(StatusCodes.BadRequest, e)
                      }
                  }
                } ~
                get {
                   onSuccess(Pictures.getPicture(pictureId).interpret(pictureInterpreter)) {
                     case Some(p) => complete(p)
                     case None => complete(StatusCodes.NotFound)
                   }
                } ~
                put {
                  decodeRequest {
                    entity(as[Pictures.Picture]) { picture =>
                      onSuccess(Pictures.updatePicture(picture).interpret(pictureInterpreter)) {
                        case Success(_) => complete(StatusCodes.Created)
                        case Failure(e) => complete(StatusCodes.BadRequest, e)
                      }
                    }
                  }
                } ~
                delete {
                  onSuccess(Pictures.deletePicture(pictureId).interpret(pictureInterpreter)) { _ =>
                    complete(StatusCodes.NoContent)
                  }
                }
              } ~
              path("_action") {
                post {
                  decodeRequest {
                    entity(as[Action]) { action =>
                      onSuccess(Pictures.rotatePicture(pictureId, action.rotation).interpret(pictureInterpreter)) {
                        complete(StatusCodes.OK)
                      }
                    }
                  }
                }
              }
            }
          } ~
          path("thumbnails") {
            pathEnd {
              get {
                onSuccess(Pictures.getThumbnailsByAlbum(albumId).interpret(pictureInterpreter)) { pictures =>
                  complete(pictures)
                }
              }
            }
          }
        }
      }
    }
}
