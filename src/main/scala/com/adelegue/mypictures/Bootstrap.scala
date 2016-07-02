package com.adelegue.mypictures

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.stream.ActorMaterializer
import com.adelegue.mypictures.domains.Api

/**
  * Created by adelegue on 23/05/2016.
  */
object Bootstrap extends App {

  implicit val system = ActorSystem("MyPictures")
  implicit val materializer = ActorMaterializer()
  implicit val executionContext = system.dispatcher

  val bindingFuture = Http().bindAndHandle(Api().route, "localhost", 8080)

  println(s"Server online at http://localhost:8080/")
//  bindingFuture
//    .flatMap(_.unbind()) // trigger unbinding from the port
//    .onComplete(_ => system.terminate()) // and shutdown when done

}
