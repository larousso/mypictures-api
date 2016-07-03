package com.adelegue.mypictures

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.stream.ActorMaterializer
import com.adelegue.mypictures.domains.Api
import com.adelegue.mypictures.domains.account.impl.AccountInterpreter
import com.adelegue.mypictures.domains.album.impl.AlbumInterpreter
import com.adelegue.mypictures.domains.picture.impl.{ImagesInterpreter, PicturesInterpreter}

/**
  * Created by adelegue on 23/05/2016.
  */
object Bootstrap extends App {

  implicit val system = ActorSystem("MyPictures")
  implicit val materializer = ActorMaterializer()
  import system.dispatcher

  val api = Api(AccountInterpreter(system), AlbumInterpreter(system), ImagesInterpreter(""), PicturesInterpreter(system))
  val bindingFuture = Http().bindAndHandle(api.route, "localhost", 8080)

  println(s"Server online at http://localhost:8080/")
//  bindingFuture
//    .flatMap(_.unbind()) // trigger unbinding from the port
//    .onComplete(_ => system.terminate()) // and shutdown when done

}
