package com.adelegue.mypictures

import akka.actor.ActorSystem
import akka.event.Logging
import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.{MalformedRequestContentRejection, RejectionHandler, ValidationRejection}
import akka.stream.ActorMaterializer
import com.adelegue.mypictures.domains.Api
import com.adelegue.mypictures.domains.account.Accounts
import com.adelegue.mypictures.domains.album.Albums
import com.adelegue.mypictures.domains.comment.Comments
import com.adelegue.mypictures.domains.picture.{Images, Pictures}
import com.adelegue.mypictures.validation.Validation
import com.typesafe.config.ConfigFactory
import org.json4s.{DefaultFormats, jackson}

/**
  * Created by adelegue on 23/05/2016.
  */
object Logger {
  def logger(implicit system: ActorSystem) = Logging.getLogger(system, "application")
}

object Bootstrap extends App {
  import ch.megard.akka.http.cors.CorsDirectives._
  val config = ConfigFactory.load()

  implicit val system = ActorSystem("MyPictures")
  implicit val materializer = ActorMaterializer()

  implicit def myRejectionHandler = RejectionHandler.newBuilder()
    .handle { case ValidationRejection(msg, cause) ⇒
      complete((StatusCodes.BadRequest, "That wasn't valid! " + msg))
    }
    .handle { case MalformedRequestContentRejection(msg, cause) ⇒
      Logger.logger.error(s"$msg, $cause")
      import de.heikoseeberger.akkahttpjson4s.Json4sSupport._
      implicit val serialization = jackson.Serialization
      implicit val formats = DefaultFormats
      cors() {
        complete(StatusCodes.BadRequest -> Validation.Error(cause.getMessage))
      }
    }
    .result()



  import system.dispatcher

  val port = config.getInt("app.port")
  val host = config.getString("app.host")

  private val accounts: Accounts = new Accounts()
  private val albums: Albums = new Albums(accounts)
  private val images: Images = new Images(config.getString("app.images.path"))
  private val pictures: Pictures = new Pictures(albums, images)
  private val comments: Comments = new Comments(pictures)

  val api = Api(config, accounts, albums, images, pictures, comments)
  val bindingFuture = Http().bindAndHandle(api.route, host, port)

  Logger.logger.info(s"Server online at http://{}:{}", host, port)

}
