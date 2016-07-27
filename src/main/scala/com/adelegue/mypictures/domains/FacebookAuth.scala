package com.adelegue.mypictures.domains

import akka.NotUsed
import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.Http.HostConnectionPool
import akka.http.scaladsl.model.{ContentTypes, HttpRequest, HttpResponse}
import akka.http.scaladsl.unmarshalling.Unmarshal
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.{Flow, Sink, Source}
import com.adelegue.mypictures.Logger
import org.json4s.JsonAST.JObject
import org.json4s.{DefaultFormats, jackson}

import scala.concurrent.Future
import scala.util.Try

/**
  * Created by adelegue on 04/07/2016.
  */
class FacebookAuth(redirectUrl: String, appId: String, appSecret: String)(implicit val system: ActorSystem) {

  import system.dispatcher
  import de.heikoseeberger.akkahttpjson4s.Json4sSupport._
  implicit val serialization = jackson.Serialization
  implicit val formats = DefaultFormats

  implicit val materializer = ActorMaterializer()
  private val httpClient: Flow[(HttpRequest, NotUsed), (Try[HttpResponse], NotUsed), HostConnectionPool] = Http().cachedHostConnectionPoolHttps[NotUsed](host = "graph.facebook.com")

  Logger.logger.info("Facebook auth redierct : {}, appId : {}", redirectUrl, appId)

  val redirectUri = s"http://$redirectUrl/auth/facebook/callback"
  val auth = s"https://www.facebook.com/dialog/oauth?client_id=$appId&redirect_uri=$redirectUri"

  def me(token: String) = {
    val uri = s"/me?access_token=$token"
    Source.single((HttpRequest(uri = uri), NotUsed))
      .via(httpClient)
      .map(_._1.get)
      .mapAsync(1)(response => Unmarshal(response.entity.withContentType(ContentTypes.`application/json`)).to[JObject])
      .runWith(Sink.head)
  }

  def accessToken(code: String) : Future[JObject] = {
    val uri = s"/v2.3/oauth/access_token?client_id=$appId&redirect_uri=$redirectUri&client_secret=$appSecret&code=$code"
    Source.single((HttpRequest(uri = uri), NotUsed))
        .via(httpClient)
        .map(_._1.get)
        .mapAsync(1)(response => Unmarshal(response.entity).to[JObject])
        .runWith(Sink.head)
  }


}
