package com.adelegue.mypictures.domains.album

import java.util.Date

import scala.concurrent._
import com.adelegue.mypictures.domains.account.Accounts
import com.adelegue.mypictures.domains.account.Accounts.Role.Guest
import com.adelegue.mypictures.{AkkaPersistanceSpecs2, PersistenceConfig}
import org.specs2.mutable.Specification
import com.adelegue.mypictures.validation.Validation.Error

import scala.concurrent.duration.DurationInt
import scalaz.Scalaz._
import cats.~>
import cats.Id
import freek._
import Albums._
import com.adelegue.mypictures.domains.account.Accounts.{Account, GetAccountByUsername}
import com.adelegue.mypictures.domains.album.impl.AlbumInterpreter

import scalaz.{Failure, NonEmptyList, Success}


/**
  * Created by adelegue on 26/05/2016.
  */
class AlbumsSpec extends Specification {


  object Futured extends (cats.Id ~> Future) {
    def apply[A](a: cats.Id[A]) = Future.successful(a)
  }

  def await[T](f : Future[T]) = Await.result(f, 1 minute)

  "Account store" should {

    "add album ok" in new AkkaPersistanceSpecs2(PersistenceConfig()) {

      import cats.std.future._
      import ExecutionContext.Implicits.global

      val accountInterpreterTest = new (Accounts.DSL ~> Id) {
        override def apply[A](fa: Accounts.DSL[A]): Id[A] = fa match {
          case GetAccountByUsername(username) => Some(Account("username", "password", "name", "surname", Guest)).asInstanceOf[Id[A]]
        }
      }

      val idToFuture: ~>[Accounts.DSL, Future] = Futured compose accountInterpreterTest
      val interpreters =  AlbumInterpreter(system) :&: idToFuture


      val album = Album("123456", "username", "un titre", "une description".some, new Date())
      val event = await(Albums.createAlbum(album).interpret(interpreters))

      event mustEqual Success(AlbumCreated(album))

      val album2 = Album("234567", "username2", "un titre", None, new Date())
      val event2 = await(Albums.createAlbum(album2).interpret(interpreters))

      event2 mustEqual Success(AlbumCreated(album2))

      await(Albums.getAlbum("123456").interpret(interpreters)) must beSome(album)
      await(Albums.getAlbumByUsername("username").interpret(interpreters)) mustEqual List(album)
      await(Albums.getAlbum("234567").interpret(interpreters)) must beSome(album2)
      await(Albums.getAlbumByUsername("username2").interpret(interpreters)) mustEqual List(album2)
      await(Albums.listAll.interpret(interpreters)) mustEqual List(album, album2)
    }

    "add album with validation error" in new AkkaPersistanceSpecs2(PersistenceConfig()) {

      import cats.std.future._
      import ExecutionContext.Implicits.global

      val accountInterpreterTest = new (Accounts.DSL ~> Id) {
        override def apply[A](fa: Accounts.DSL[A]): Id[A] = fa match {
          case GetAccountByUsername(username) => username match {
            case "username" => Some(Account("username", "password", "name", "surname", Guest)).asInstanceOf[Id[A]]
            case _ => None.asInstanceOf[Id[A]]
          }
        }
      }

      val idToFuture: ~>[Accounts.DSL, Future] = Futured compose accountInterpreterTest
      val interpreters = idToFuture :&: AlbumInterpreter(system)

      val album = Album("123456", "username", "un titre", "une description".some, new Date())
      val event = await(Albums.createAlbum(album).interpret(interpreters))

      event mustEqual Success(AlbumCreated(album))

      val album2 = Album("123456", "username2", "un titre", None, new Date())
      val event2 = await(Albums.createAlbum(album2).interpret(interpreters))

      event2 mustEqual Failure(NonEmptyList(Error("L'utilisateur username2 n'existe pas"), Error("L'album existe déjà")))

      await(Albums.getAlbum("123456").interpret(interpreters)) must beSome(album)
      await(Albums.listAll.interpret(interpreters)) mustEqual List(album)
    }

    "update album ok" in new AkkaPersistanceSpecs2(PersistenceConfig()) {
      import cats.std.future._
      import ExecutionContext.Implicits.global

      val accountInterpreterTest = new (Accounts.DSL ~> Id) {
        override def apply[A](fa: Accounts.DSL[A]): Id[A] = fa match {
          case GetAccountByUsername(username) => Some(Account("username", "password", "name", "surname", Guest)).asInstanceOf[Id[A]]
        }
      }

      val idToFuture = Futured compose accountInterpreterTest
      val interpreters = idToFuture :&: AlbumInterpreter(system)

      val album = Album("123456", "username", "un titre", "une description".some, new Date())
      val event = await(Albums.createAlbum(album).interpret(interpreters))

      event mustEqual Success(AlbumCreated(album))

      val album2 = Album("123456", "username", "un titre2", None, new Date())
      val event2 = await(Albums.updateAlbum(album2).interpret(interpreters))

      event2 mustEqual Success(AlbumUpdated(album2))

      await(Albums.getAlbum("123456").interpret(interpreters)) must beSome(album2)
      await(Albums.getAlbumByUsername("username").interpret(interpreters)) mustEqual List(album2)
      await(Albums.listAll.interpret(interpreters)) mustEqual List(album2)
    }

    "update album with validation error" in new AkkaPersistanceSpecs2(PersistenceConfig()) {
      import cats.std.future._
      import ExecutionContext.Implicits.global

      val accountInterpreterTest = new (Accounts.DSL ~> Id) {
        override def apply[A](fa: Accounts.DSL[A]): Id[A] = fa match {
          case GetAccountByUsername(username) => None.asInstanceOf[Id[A]]
        }
      }

      val idToFuture = Futured compose accountInterpreterTest
      val interpreters = idToFuture :&: AlbumInterpreter(system)

      val album = Album("123456", "username", "un titre", "une description".some, new Date())
      val event = await(Albums.updateAlbum(album).interpret(interpreters))

      event mustEqual Failure(NonEmptyList(Error("L'utilisateur username n'existe pas"), Error("L'album n'existe pas")))

      await(Albums.getAlbum("123456").interpret(interpreters)) must beNone
      await(Albums.listAll.interpret(interpreters)) mustEqual List.empty[Album]
    }


    "delete album ok" in new AkkaPersistanceSpecs2(PersistenceConfig()) {
      import cats.std.future._
      import ExecutionContext.Implicits.global

      val accountInterpreterTest = new (Accounts.DSL ~> Id) {
        override def apply[A](fa: Accounts.DSL[A]): Id[A] = fa match {
          case GetAccountByUsername(username) => Some(Account("username", "password", "name", "surname", Guest)).asInstanceOf[Id[A]]
        }
      }

      val idToFuture = Futured compose accountInterpreterTest
      val interpreters = idToFuture :&: AlbumInterpreter(system)


      val album = Album("123456", "username", "un titre", "une description".some, new Date())
      val event = await(Albums.createAlbum(album).interpret(interpreters))

      event mustEqual Success(AlbumCreated(album))

      val event2 = await(Albums.deleteAlbum("123456").interpret(interpreters))

      event2 mustEqual Success(AlbumDeleted("123456"))

      await(Albums.getAlbum("123456").interpret(interpreters)) must beNone
      await(Albums.listAll.interpret(interpreters)) mustEqual List.empty[Album]

    }



  }

}
