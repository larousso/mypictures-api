package com.adelegue.mypictures

import cats._
import cats.free.Free
import org.specs2.mutable.Specification

import scala.concurrent.{Await, Future}
import scala.concurrent._
import scala.concurrent.duration._
import freek._

/**
  * Created by adelegue on 06/06/2016.
  */
class FreeMonadSpec extends Specification {

  "free monad" should {
      "free" should {
        import cats.std.future._
        import ExecutionContext.Implicits.global

        val interpreters = Api1Interpreter :&: ApiInterpreter
        val map =  Await.result(Api1.foo("1").foldMap(interpreters.nat), 1 second)

        map must beEqualTo(Api1.Response("Oh yeah 1 Oh yeah 1"))
      }
  }

  object Api {


    type PGR = DSL :|: FXNil
    val PRG = Program[PGR]

    def foo(a: String) =
      for {
        f <- Foo(a).freek[PGR]
      } yield f

    def bar() =
      for {
        f <- Bar().freek[PGR]
      } yield f

    sealed trait DSL[A]
    case class Response(message: String)
    case class Foo(a: String) extends DSL[Response]
    case class Bar() extends DSL[String]
  }

  object ApiInterpreter extends (Api.DSL ~> Future) {
    import Api._
    override def apply[A](fa: Api.DSL[A]): Future[A] = fa match {
      case Foo(a) => Future.successful(Response(s"Oh yeah $a")).asInstanceOf[Future[A]]
      case Bar() => Future.successful("Youhou").asInstanceOf[Future[A]]
    }
  }

  object Api1 {

    type PRG = Api1.DSL :|: Api.PGR
    val PRG = Program[PRG]

    def foo(a: String): Free[PRG.Cop, Response] = for {
        r <- Api.foo(a).expand[PRG]
        a <- Foo(r.message).freek[PRG]
    } yield a

    def bar(): Free[PRG.Cop, String] = for { b <- Bar().freek[PRG] } yield b

    case class Response(message: String)
    sealed trait DSL[A]
    case class Foo(a: String) extends DSL[Response]
    case class Bar() extends DSL[String]
  }

  object Api1Interpreter extends (Api1.DSL ~> Future) {
    import Api1._
    override def apply[A](fa: Api1.DSL[A]): Future[A] = fa match {
      case Foo(a) => Future.successful(Response(s"Oh yeah 1 $a")).asInstanceOf[Future[A]]
      case Bar() => Future.successful("Youhou").asInstanceOf[Future[A]]
    }
  }
}
