package com.adelegue.mypictures.domains

import java.text.SimpleDateFormat

import akka.actor.ActorSystem
import akka.http.scaladsl.model.HttpMethods._
import akka.http.scaladsl.model._
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import akka.stream.Materializer
import cats._
import cats.std.future._
import ch.megard.akka.http.cors.CorsDirectives._
import ch.megard.akka.http.cors.CorsSettings
import com.adelegue.mypictures.domains.account.Accounts
import com.adelegue.mypictures.domains.account.Accounts.{Role, _}
import com.adelegue.mypictures.domains.album.Albums
import com.adelegue.mypictures.domains.comment.Comments
import com.adelegue.mypictures.domains.picture.Images.RotationSerializer
import com.adelegue.mypictures.domains.picture.{Images, Pictures}
import com.typesafe.config.Config
import de.heikoseeberger.akkahttpjson4s.Json4sSupport._
import freek._
import org.json4s.JsonAST.JString
import org.json4s.{CustomSerializer, DefaultFormats, jackson}

import scala.collection.JavaConversions._
import scala.collection.immutable
import scala.concurrent.Future

/**
  * Created by adelegue on 30/06/2016.
  */
object Api {

  case class LoginForm(username: Option[String], password: Option[String]) extends Serializable
  case class SessionUser(username: Username, role: Role, sessionType: SessionType)
  case class Session(user: Option[SessionUser], redirect: Option[String] = None)

  sealed trait SessionType { def sessionType: String }
  case object Facebook extends SessionType { override val sessionType = "facebook" }

  case object Custom extends SessionType { override val sessionType = "custom" }

  class SessionTypeSerializer extends CustomSerializer[SessionType](format => ({
    case JString(s) if s == Facebook.sessionType =>
      Facebook
    case JString(s) if s == Custom.sessionType =>
      Custom
  }, {
    case n: SessionType =>
      JString(n.sessionType)
  }))

  def apply(
             config: Config,
             accounts: Accounts,
             albums: Albums,
             images: Images,
             pictures: Pictures,
             comments: Comments)(implicit system: ActorSystem, materializer: Materializer): Api =
    new Api(config, accounts, albums, images, pictures, comments)(system, materializer)
}

class Api(
           config: Config,
           accounts: Accounts,
           albums: Albums,
           images: Images,
           pictures: Pictures,
           comments: Comments)
         (implicit system: ActorSystem, materializer: Materializer) {


  import system.dispatcher
  implicit val serialization = jackson.Serialization
  implicit val formats = new DefaultFormats { override def dateFormatter = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSS'Z'") } + new RoleSerializer + new RotationSerializer


  private val users = config.getConfigList("users").toList
  users.foreach( userConfig => {
    accounts.createOrUpdateAccount(Accounts.Account(userConfig.getString("username"), userConfig.getString("password"), userConfig.getString("name"), userConfig.getString("surname"), Role.fromString(userConfig.getString("role"))))
  })

  val auth = Auth(config, accounts)

  val corsSettings = CorsSettings.defaultSettings.copy(allowedMethods = immutable.Seq(GET, POST, HEAD, OPTIONS, PUT, DELETE))

  val accountApi = Accounts.Api(accounts)
  val albumApi = Albums.Api(auth, albums, pictures)
  val pictureApi = Pictures.Api(auth, pictures)
  val commentApi = Comments.Api(comments)

  def route() : Route = {
    cors(corsSettings) {
      auth.facebookApi ~
        pathPrefix("static") {
          pictureApi.readImage() ~
            pictureApi.readThumbnails()
        } ~
        pathPrefix("api") {
          auth.loginApi ~
          auth.isAuthenticated {
            path("pictures" / "[a-z0-9\\-]+".r) { pictureId: Pictures.Id =>
              pictureApi.readPicture(pictureId)
            } ~
            pathPrefix("albums" / "[a-z0-9\\-]+".r) { albumId: Albums.Id =>
              albumApi.readAlbum(albumId) ~
                path("pictures") {
                  pathEnd {
                    pictureApi.readPictures(albumId)
                  } ~
                    path("[a-z0-9\\-]+".r) { pictureId: Pictures.Id =>
                      get {
                        onSuccess(pictures.getPicture(pictureId)) {
                          case Some(p) => complete(p)
                          case None => complete(StatusCodes.NotFound)
                        }
                      }
                    }
                }
            } ~
              accountApi.route { username =>
                albumApi.route(username) { albumId =>
                  pictureApi.route(albumId) { pictureId =>
                    commentApi.route(pictureId)
                  }
                }
              }
          }
        }
    }
  }

}
