package com.adelegue.mypictures

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.{MalformedRequestContentRejection, RejectionHandler, ValidationRejection}
import akka.stream.ActorMaterializer
import com.adelegue.mypictures.domains.Api
import com.adelegue.mypictures.domains.account.impl.AccountInterpreter
import com.adelegue.mypictures.domains.album.impl.AlbumInterpreter
import com.adelegue.mypictures.domains.comment.impl.CommentInterpreter
import com.adelegue.mypictures.domains.picture.impl.{ImagesInterpreter, PicturesInterpreter}
import com.adelegue.mypictures.validation.Validation
import com.typesafe.config.ConfigFactory
import org.json4s.{DefaultFormats, jackson}

/**
  * Created by adelegue on 23/05/2016.
  */
object Bootstrap extends App {
  import ch.megard.akka.http.cors.CorsDirectives._

  implicit def myRejectionHandler = RejectionHandler.newBuilder()
    .handle { case ValidationRejection(msg, cause) ⇒
      complete((StatusCodes.BadRequest, "That wasn't valid! " + msg))
    }
    .handle { case MalformedRequestContentRejection(msg, cause) ⇒
      println(s"$msg, $cause")
      import de.heikoseeberger.akkahttpjson4s.Json4sSupport._
      implicit val serialization = jackson.Serialization
      implicit val formats = DefaultFormats
      cors() {
        complete(StatusCodes.BadRequest -> Validation.Error(cause.getMessage))
      }
    }
    .result()


  val config = ConfigFactory.load()

  implicit val system = ActorSystem("MyPictures")
  implicit val materializer = ActorMaterializer()
  import system.dispatcher

  val port = config.getInt("app.port")
  val host = config.getString("app.host")

  val api = Api(config, AccountInterpreter(system), AlbumInterpreter(system), ImagesInterpreter("target/images"), PicturesInterpreter(system), CommentInterpreter(system))
  val bindingFuture = Http().bindAndHandle(api.route, host, port)

  println(s"Server online at http://$host:$port/")
//  bindingFuture
//    .flatMap(_.unbind()) // trigger unbinding from the port
//    .onComplete(_ => system.terminate()) // and shutdown when done

}
