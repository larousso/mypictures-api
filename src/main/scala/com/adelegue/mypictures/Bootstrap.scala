package com.adelegue.mypictures

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.stream.ActorMaterializer
import com.adelegue.mypictures.domains.Api
import com.adelegue.mypictures.domains.account.impl.AccountInterpreter
import com.adelegue.mypictures.domains.album.impl.AlbumInterpreter
import com.adelegue.mypictures.domains.picture.impl.{ImagesInterpreter, PicturesInterpreter}
import com.typesafe.config.ConfigFactory

/**
  * Created by adelegue on 23/05/2016.
  */
object Bootstrap extends App {

  val config = ConfigFactory.load()

  implicit val system = ActorSystem("MyPictures")
  implicit val materializer = ActorMaterializer()
  import system.dispatcher

  val port = config.getInt("app.port")
  val host = config.getString("app.host")

  val api = Api(config, AccountInterpreter(system), AlbumInterpreter(system), ImagesInterpreter(""), PicturesInterpreter(system))
  val bindingFuture = Http().bindAndHandle(api.route, host, port)

  println(s"Server online at http://$host:$port/")
//  bindingFuture
//    .flatMap(_.unbind()) // trigger unbinding from the port
//    .onComplete(_ => system.terminate()) // and shutdown when done

}
