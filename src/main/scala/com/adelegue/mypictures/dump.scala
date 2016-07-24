package com.adelegue.mypictures

import java.text.SimpleDateFormat

import scala.concurrent.Future
import scala.collection.immutable.Seq
import akka.actor.ActorSystem
import akka.stream.scaladsl._
import akka.stream.{ActorMaterializer, Materializer}
import akka.stream.ActorMaterializerSettings
import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.http.scaladsl.model.headers._
import com.adelegue.mypictures.domains.account.Accounts
import com.adelegue.mypictures.domains.account.Accounts._
import com.adelegue.mypictures.domains.album.Albums
import com.adelegue.mypictures.domains.picture._
import akka.http.scaladsl.unmarshalling._
import java.util.Base64

import cats._
import com.typesafe.config.Config
import freek._

object Dump {

  def run(config: Config, acc: Accounts.DSL ~> Future, alb: Albums.DSL ~> Future, pict: Pictures.DSL ~> Future, img: Images.DSL ~> Future)(implicit system: ActorSystem) = {

    import cats.std.future._
    import system.dispatcher
    implicit val materializer = ActorMaterializer(ActorMaterializerSettings(system))
    val d = new Dump()

    val accountInterpreter = acc
    val albumInterpreter = alb :&: accountInterpreter
    val pictureInterpreter = pict :&: img :&: albumInterpreter

    val username = config.getString("admin.username")
    val password = config.getString("admin.password")
    Logger.logger.info("Starting copy with user {}", username)
    Source
      .fromFuture(d.login(username, password))
      .flatMapMerge(1,  cookies => {
        Logger.logger.info("Getting album for heloise with auth {}", cookies)
        Source.fromFuture(d.findAlbums("heloise", cookies))
            .mapConcat(identity _)
            .mapAsyncUnordered(4){alb =>
              Logger.logger.info("Saving album {}", alb)
              Albums.createAlbum(alb).interpret(albumInterpreter)
            }
            .mapAsyncUnordered(4) {
              case scalaz.Success(created) =>
                Logger.logger.info("Getting pictures for album {}", created.album.id)
                d.findPictures("heloise", created.album.id, cookies)
              case scalaz.Failure(f) =>
                Logger.logger.info("Error saving album {}", f)
                Future.failed(new RuntimeException("Error saving album"))
            }
            .mapConcat(identity _)
            .mapAsyncUnordered(4) {
              case (Some(file), picture) =>
                Logger.logger.info(s"Saving picture {}", picture)
                Pictures.createPicture(picture, file).interpret(pictureInterpreter)
              case _ =>
                Future.failed(new RuntimeException("Fuck"))
            }
        })
      .runWith(Sink.seq)
  }

}

class Dump()(implicit system: ActorSystem, materializer: Materializer) {
    import org.json4s._
    import system.dispatcher
    import de.heikoseeberger.akkahttpjson4s.Json4sSupport._
    implicit val serialization = native.Serialization

    //val pictureSerializer = FieldSerializer[Pictures.Picture](deserializer = ignore("thumbnail") orElse ignore("file"))

    implicit val formats = new DefaultFormats { override def dateFormatter = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSS'Z'") } + new RoleSerializer

    val http = Http(system)
    val baseUrl = "http://vps244493.ovh.net/api"

    def findAlbums(username: String, cookies: Seq[HttpCookiePair]): Future[Seq[Albums.Album]] = {
      Logger.logger.info(s"cookies $cookies")
      val accountsUrl = s"$baseUrl/accounts/$username/albums"
      get[Seq[Albums.Album]](accountsUrl, cookies)
    }

    def findPictures(username: String, albumId: String, cookies: Seq[HttpCookiePair]): Future[Seq[(Option[Array[Byte]], Pictures.Picture)]] = {
      val picturesUrl = s"$baseUrl/accounts/$username/albums/$albumId/pictures"
      get[JValue](picturesUrl, cookies).map {
        case JArray(pictures) => pictures.map { p =>
            val picture = p.extract[Pictures.Picture]
            for {
              JObject(child) <- p
              JField("file", JString(file)) <- child
            } yield (decodeImage(file), picture)
          }.flatten.toSeq
        case _ => Seq.empty[(Option[Array[Byte]], Pictures.Picture)]
      }
    }

    val base64 = "data:image/jpeg;base64, (.*)".r

    def decodeImage(file: String):Option[Array[Byte]] = {
      file match {
        case base64(data) =>
          Some( Base64.getDecoder.decode(data.getBytes("utf-8")) )
        case _ =>
          None
      }
    }

    def get[T](url: String, cookies: Seq[HttpCookiePair])(implicit um: Unmarshaller[ResponseEntity, T]): Future[T] = { //(implicit formats: Formats, manifest: Manifest[T]): Future[T] = {
      http.singleRequest(HttpRequest(uri = url, headers = Seq(Cookie(cookies)), entity = HttpEntity(MediaTypes.`application/json`, "")))
        .flatMap {
            case HttpResponse(StatusCodes.OK, headers, entity, _) =>
            Unmarshal(entity).to[T]
            case _ =>
              Future.failed(new RuntimeException("Oups"))
        }
    }


    def login(username: String, password: String): Future[Seq[HttpCookiePair]] = {
      http.singleRequest(HttpRequest(
        uri = s"$baseUrl/login",
        method= HttpMethods.POST,
        entity = HttpEntity(MediaTypes.`application/json`, s"""{"username": "$username", "password": "$password"}"""))
      )
      .flatMap {
        case HttpResponse(StatusCodes.OK, headers, entity, _) =>
          val cookies = headers.collect { case `Set-Cookie`(x) ⇒ x}.map {_.pair}
          entity
            .dataBytes
            .map { _.utf8String}
            .runFold("")(_ ++ _)
            .map { str =>
              Logger.logger.info(s"Auth : $str")
              cookies
            }
        case HttpResponse(status, headers, entity, _) =>
         entity
           .dataBytes
           .map { _.utf8String}
           .runFold("")(_ ++ _)
           .map { str =>
             Logger.logger.info(s"Auth fail : $status, $str")
             Seq.empty[HttpCookiePair]
           }
      }
  }

}
