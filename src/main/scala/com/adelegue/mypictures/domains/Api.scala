package com.adelegue.mypictures.domains

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Directives._
import akka.stream.Materializer
import cats._
import com.adelegue.mypictures.domains.account.Accounts
import com.adelegue.mypictures.domains.account.Accounts.Role.Admin
import com.adelegue.mypictures.domains.account.Accounts.{Role, _}
import com.adelegue.mypictures.domains.album.Albums
import com.adelegue.mypictures.domains.picture.{Images, Pictures}
import org.json4s.JsonAST.{JField, JObject, JString}
import org.json4s.{CustomSerializer, DefaultFormats, jackson}

import scala.concurrent.{ExecutionContext, Future}
import scalaz.{Failure, Success}

/**
  * Created by adelegue on 30/06/2016.
  */
object Api {

  case class Action(rotation: Images.Rotation) extends Serializable
  case class LoginForm(login: String, password: String) extends Serializable

  case class Session(username: Username, role: Role, sessionType: SessionType)

  sealed trait SessionType {
    def sessionType: String
  }

  case object Facebook extends SessionType {
    override val sessionType = "facebook"
  }

  case object Custom extends SessionType {
    override val sessionType = "custom"
  }

  class SessionTypeSerializer extends CustomSerializer[SessionType](format => (
  {
    case JString(s) if s == Facebook.sessionType =>
      Facebook
    case JString(s) if s == Custom.sessionType =>
      Custom
  }, {
    case n: SessionType =>
      JString(n.sessionType)
  }))

  def apply(acc: Accounts.DSL ~> Future, alb: Albums.DSL ~> Future, img: Images.DSL ~> Future, pict: Pictures.DSL ~> Future)(implicit ec: ExecutionContext): Api = new Api(acc, alb, img, pict)(ec)
}

class Api(acc: Accounts.DSL ~> Future, alb: Albums.DSL ~> Future, img: Images.DSL ~> Future, pict: Pictures.DSL ~> Future)(implicit ec: ExecutionContext) {

  import cats.std.future._
  import freek._

  val accountInterpreter = acc
  val albumInterpreter = alb :&: accountInterpreter
  val pictureInterpreter = pict :&: img :&: albumInterpreter

  val init = for {
    _ <- Accounts.createOrUpdateAccount(Accounts.Account("alex", "alex", "alex", "alex", Admin))
  } yield ()

  init.interpret(Interpreter(accountInterpreter))

  def route(implicit materializer: Materializer) = {
    val auth = Auth(acc)
    import de.heikoseeberger.akkahttpjson4s.Json4sSupport._
    implicit val serialization = jackson.Serialization
    implicit val formats = DefaultFormats

    pathPrefix("api") {
        pathEnd {
          get {
            complete("Ok")
          }
        } ~
          auth.loginApi ~
          pathPrefix("accounts" / "\\w+".r) { username: Accounts.Username =>
            pathEnd {
              get {
                onSuccess(Accounts.getAccountByUsername(username).interpret(Interpreter(accountInterpreter))) {
                  case Some(a) => complete(a)
                  case None => complete(StatusCodes.NotFound)
                }
              }
            } ~
              pathPrefix("albums") {
                pathEnd {
                  get {
                    onSuccess(Albums.getAlbumByUsername(username).interpret(albumInterpreter)) { albums =>
                      complete(albums)
                    }
                  } ~
                    post {
                      entity(as[Albums.Album]) { album =>
                        onSuccess(Albums.createAlbum(album).interpret(albumInterpreter)) {
                          case Success(_) => complete(StatusCodes.Created)
                          case Failure(e) => complete(StatusCodes.BadRequest, e)
                        }
                      }
                    }
                } ~
                  pathPrefix("\\w+".r) { albumId: Albums.Id =>
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
                          entity(as[Albums.Album]) { album =>
                            onSuccess(Albums.updateAlbum(album).interpret(albumInterpreter)) {
                              case Success(_) => complete(StatusCodes.Created)
                              case Failure(e) => complete(StatusCodes.BadRequest, e)
                            }
                          }
                        }
                    } ~
                      pathPrefix("pictures") {
                        pathEnd {
                          get {
                            onSuccess(Pictures.getPictureByAlbum(albumId).interpret(pictureInterpreter)) { pictures =>
                              complete(pictures)
                            }
                          }
                        } ~
                          pathPrefix("\\w+".r) { pictureId: Pictures.Id =>
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
                                  entity(as[Pictures.Picture]) { picture =>
                                    onSuccess(Pictures.updatePicture(picture).interpret(pictureInterpreter)) {
                                      case Success(_) => complete(StatusCodes.Created)
                                      case Failure(e) => complete(StatusCodes.BadRequest, e)
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
                                  entity(as[Api.Action]) { action =>
                                    onSuccess(Pictures.rotatePicture(pictureId, action.rotation).interpret(pictureInterpreter)) {
                                      complete(StatusCodes.OK)
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
  }
}
