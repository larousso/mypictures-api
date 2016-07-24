package com.adelegue.mypictures.domains

import java.text.SimpleDateFormat
import java.util
import java.util.{Date, UUID}

import akka.actor.ActorSystem
import akka.http.scaladsl.model.HttpMethods._
import akka.http.scaladsl.model._
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import akka.stream.Materializer
import akka.util.ByteString
import cats._
import ch.megard.akka.http.cors.CorsSettings
import com.adelegue.mypictures.domains.account.Accounts
import com.adelegue.mypictures.domains.account.Accounts.Role.Admin
import com.adelegue.mypictures.domains.account.Accounts.{Role, _}
import com.adelegue.mypictures.domains.album.Albums
import com.adelegue.mypictures.domains.comment.Comments
import com.adelegue.mypictures.domains.picture.Images.RotationSerializer
import com.adelegue.mypictures.domains.picture.{Images, Pictures}
import com.typesafe.config.Config
import org.json4s.JsonAST.JString
import org.json4s.{CustomSerializer, DefaultFormats, jackson}

import scala.collection.immutable
import scala.concurrent.Future
import scalaz.{Failure, Success}
import com.adelegue.mypictures.{Dump, Logger}
/**
  * Created by adelegue on 30/06/2016.
  */
object Api {

  case class RotationAction(rotation: Images.Rotation) extends Serializable
  case class LoginForm(username: Option[String], password: Option[String]) extends Serializable

  case class SessionUser(username: Username, role: Role, sessionType: SessionType)
  case class Session(user: Option[SessionUser], redirect: Option[String] = None)

  case class Album(title: Albums.Title, description: Option[Albums.Description], date: Date = new Date())
  case class Comment(name: String, comment: String, date: Date = new Date())

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

  def apply(config: Config, acc: Accounts.DSL ~> Future, alb: Albums.DSL ~> Future, img: Images.DSL ~> Future, pict: Pictures.DSL ~> Future, comm: Comments.DSL ~> Future)(implicit system: ActorSystem, materializer: Materializer): Api = new Api(config, acc, alb, img, pict, comm)(system, materializer)
}

class Api(config: Config, acc: Accounts.DSL ~> Future, alb: Albums.DSL ~> Future, img: Images.DSL ~> Future, pict: Pictures.DSL ~> Future, comm: Comments.DSL ~> Future)(implicit system: ActorSystem, materializer: Materializer) {

  import cats.std.future._
  import ch.megard.akka.http.cors.CorsDirectives._
  import de.heikoseeberger.akkahttpjson4s.Json4sSupport._
  import freek._
  import system.dispatcher
  import collection.JavaConversions._

  implicit val serialization = jackson.Serialization
  implicit val formats = new DefaultFormats { override def dateFormatter = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSS'Z'") } + new RoleSerializer + new RotationSerializer

  val accountInterpreter = acc
  val albumInterpreter = alb :&: accountInterpreter
  val pictureInterpreter = pict :&: img :&: albumInterpreter
  val commentInterpreter = comm :&: pictureInterpreter

  private val users = config.getConfigList("users").toList
  users.foreach( userConfig => {
    (for {
      _ <- Accounts.createOrUpdateAccount(Accounts.Account(userConfig.getString("username"), userConfig.getString("password"), userConfig.getString("name"), userConfig.getString("surname"), Role.fromString(userConfig.getString("role"))))
    } yield ()).interpret(Interpreter(accountInterpreter))
  })

  val auth = Auth(config, acc)

  val corsSettings = CorsSettings.defaultSettings.copy(allowedMethods = immutable.Seq(GET, POST, HEAD, OPTIONS, PUT, DELETE))
  def route(): Route =
    cors(corsSettings) {
      path("resynchro") {
          onComplete(Dump.run(config, acc, alb, pict, img)) {
            case scala.util.Success(res) => complete(StatusCodes.OK -> res)
            case scala.util.Failure(e) => {
              Logger.logger.error("Error during resynchro", e)
              complete(StatusCodes.BadRequest -> e)
            }
          }
      } ~
      auth.facebookApi ~
        pathPrefix("static") {
          auth.isAuthenticated {
            pathPrefix("images") {
              path("[a-z0-9\\-]+".r) { pictureId =>
                onSuccess(Pictures.readImage(pictureId).interpret(pictureInterpreter)) { byteArray =>
                  complete(HttpResponse(StatusCodes.OK, entity = HttpEntity(ContentType(MediaTypes.`image/jpeg`), byteArray)))
                }
              }
            }
          }~
          pathPrefix("thumbnails") {
            path("[a-z0-9\\-]+".r) { pictureId =>
              onSuccess(Pictures.readThumbnail(pictureId).interpret(pictureInterpreter)) { byteArray =>
                complete(HttpResponse(StatusCodes.OK, entity = HttpEntity(ContentType(MediaTypes.`image/jpeg`), byteArray)))
              }
            }
          }
        } ~
        pathPrefix("api") {
          auth.loginApi ~
            auth.isAuthenticated {
              pathPrefix("albums" / "[a-z0-9\\-]+".r) { albumId: Albums.Id =>
                pathEnd {
                  get {
                    onSuccess(Albums.getAlbum(albumId).interpret(albumInterpreter)) {
                      case Some(a) => complete(a)
                      case None => complete(StatusCodes.NotFound)
                    }
                  }
                } ~
                path("pictures") {
                  get {
                    onSuccess(Pictures.getPictureByAlbum(albumId).interpret(pictureInterpreter)) { pictures =>
                      complete(StatusCodes.OK -> pictures)
                    }
                  }
                }
              } ~
              path("pictures" / "[a-z0-9\\-]+".r) { pictureId: Pictures.Id =>
                get {
                  onSuccess(Pictures.getPicture(pictureId).interpret(pictureInterpreter)) {
                    case Some(p) => complete(p)
                    case None => complete(StatusCodes.NotFound)
                  }
                }
              } ~
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
                          auth.hashRole(Admin) {
                            entity(as[Api.Album]) {
                              case Api.Album(title, description, date) =>
                                onSuccess(Albums.createAlbum(Albums.Album(UUID.randomUUID.toString, username, title, description, date)).interpret(albumInterpreter)) {
                                  case Success(a) => complete(StatusCodes.Created -> a.album)
                                  case Failure(e) => complete(StatusCodes.BadRequest -> e)
                                }
                            }
                          }
                        }
                    } ~
                      pathPrefix("[a-z0-9\\-]+".r) { albumId: Albums.Id =>
                        pathEnd {
                          get {
                            onSuccess(Albums.getAlbum(albumId).interpret(albumInterpreter)) {
                              case Some(a) => complete(a)
                              case None => complete(StatusCodes.NotFound)
                            }
                          } ~
                            delete {
                              auth.hashRole(Admin) {
                                val res = for {
                                  _ <- Pictures.deletePicturesByAlbum(albumId)
                                  delete <- Albums.deleteAlbum(albumId).expand[Pictures.PRG]
                                } yield delete

                                onSuccess(res.interpret(pictureInterpreter)) {
                                  case Success(a) =>
                                    complete(StatusCodes.OK -> a)
                                  case Failure(e) =>
                                    complete(StatusCodes.BadRequest -> e)
                                }
                              }
                            } ~
                            put {
                              auth.hashRole(Admin) {
                                entity(as[Albums.Album]) { album =>
                                  onSuccess(Albums.updateAlbum(album).interpret(albumInterpreter)) {
                                    case Success(a) =>
                                      complete(StatusCodes.OK -> a.album)
                                    case Failure(e) =>
                                      complete(StatusCodes.BadRequest -> e)
                                  }
                                }
                              }
                            }
                        } ~
                          pathPrefix("pictures") {
                            pathEnd {
                              get {
                                onSuccess(Pictures.getPictureByAlbum(albumId).interpret(pictureInterpreter)) { pictures =>
                                  complete(StatusCodes.OK -> pictures)
                                }
                              }
                            } ~
                              pathPrefix("[a-z0-9\\-]+".r) { pictureId: Pictures.Id =>
                                pathEnd {
                                  post {
                                    auth.hashRole(Admin) {
                                      extractRequestContext { ctx =>
                                        fileUpload("file") {
                                          case (metadata, byteSource) =>
                                            val filename: Pictures.Filename = metadata.fileName
                                            metadata.contentType.mediaType.fileExtensions
                                            val imgType: Pictures.Type = "image/jpeg"
                                            val picture = Pictures.Picture(pictureId, filename, imgType, albumId)
                                            onSuccess(byteSource
                                              .runFold(ByteString.empty)(_ ++ _)
                                              .map(_.toArray[Byte])
                                              .flatMap { fileContent =>
                                                Pictures.createPicture(picture, fileContent).interpret(pictureInterpreter)
                                              }) {
                                              case Success(p) => complete(StatusCodes.Created -> p.picture)
                                              case Failure(e) => complete(StatusCodes.BadRequest -> e)
                                            }
                                        }
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
                                        case Success(p) => complete(StatusCodes.OK -> p.picture)
                                        case Failure(e) => complete(StatusCodes.BadRequest -> e)
                                      }
                                    }
                                  } ~
                                  delete {
                                    onSuccess(Pictures.deletePicture(pictureId).interpret(pictureInterpreter)) { _ =>
                                      complete(StatusCodes.NoContent)
                                    }
                                  }
                                } ~
                                path("_rotation") {
                                  post {
                                    entity(as[Api.RotationAction]) { action =>
                                      onSuccess(Pictures.rotatePicture(pictureId, action.rotation).interpret(pictureInterpreter)) {
                                        case Some(p) => complete(StatusCodes.OK -> p)
                                        case None => complete(StatusCodes.NotFound -> pictureId)
                                      }
                                    }
                                  }
                                } ~
                                pathPrefix("comments") {
                                  pathEnd {
                                    get {
                                      onSuccess(Comments.listCommentsByPicture(pictureId).interpret(commentInterpreter)) {
                                        case l => complete(l)
                                      }
                                    } ~
                                    post {
                                      entity(as[Api.Comment]) { comment =>
                                        onSuccess(Comments.createComment(Comments.Comment(UUID.randomUUID.toString, pictureId, comment.name, comment.comment, comment.date)).interpret(commentInterpreter)) {
                                          case Success(c) => complete(StatusCodes.OK -> c.comment)
                                          case Failure(e) => complete(StatusCodes.BadRequest -> e)
                                        }
                                      }
                                    }
                                  } ~
                                  path("[a-z0-9\\-]+".r) { commentId =>
                                    put {
                                      entity(as[Comments.Comment]) { comment =>
                                        onSuccess(Comments.updateComment(comment).interpret(commentInterpreter)) {
                                          case Success(c) => complete(StatusCodes.OK -> c.comment)
                                          case Failure(e) => complete(StatusCodes.BadRequest -> e)
                                        }
                                      }
                                    } ~
                                    delete {
                                      onSuccess(Comments.deleteComment(commentId).interpret(commentInterpreter)) {
                                        case Success(c) => complete(StatusCodes.OK -> c)
                                        case Failure(e) => complete(StatusCodes.BadRequest -> e)
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
        }
    }

}
