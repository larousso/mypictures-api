package services

import akka.NotUsed
import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.Http.HostConnectionPool
import akka.http.scaladsl.model.{HttpRequest, HttpResponse}
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.{Flow, Sink, Source}
import play.api.Logger
import play.api.libs.json.{JsValue, Json}

import scala.concurrent.Future
import scala.util.Try

/**
  * Created by adelegue on 04/07/2016.
  */
class FacebookAuth(redirectUrl: String, appId: String, appSecret: String)(implicit val system: ActorSystem) {

  import system.dispatcher

  implicit val materializer = ActorMaterializer()
  private val httpClient: Flow[(HttpRequest, NotUsed), (Try[HttpResponse], NotUsed), HostConnectionPool] = Http().cachedHostConnectionPoolHttps[NotUsed](host = "graph.facebook.com")

  Logger.logger.info(s"Facebook auth redirect : $redirectUrl, appId : $appId")

  val redirectUri = s"http://$redirectUrl/auth/facebook/callback"
  val auth = s"https://www.facebook.com/dialog/oauth?client_id=$appId&redirect_uri=$redirectUri"

  def me(token: String): Future[JsValue] = {
    val uri = s"/me?access_token=$token"
    Source.single((HttpRequest(uri = uri), NotUsed))
      .via(httpClient)
      .map(_._1.get)
      .mapAsync(1)(response => response
        .entity
        .dataBytes
        .map(_.utf8String)
        .runFold("")(_ ++ _)
        .map(Json.parse)
      )
      .runWith(Sink.head)
  }

  def accessToken(code: String) : Future[JsValue] = {
    val uri = s"/v2.3/oauth/access_token?client_id=$appId&redirect_uri=$redirectUri&client_secret=$appSecret&code=$code"
    Source.single((HttpRequest(uri = uri), NotUsed))
        .via(httpClient)
        .map(_._1.get)
        .mapAsync(1)(response => response
          .entity
          .dataBytes
          .map(_.utf8String)
          .runFold("")(_ ++ _)
          .map(Json.parse)
        )
        .runWith(Sink.head)
  }


}
