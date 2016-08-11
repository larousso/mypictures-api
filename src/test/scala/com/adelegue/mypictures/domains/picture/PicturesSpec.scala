package com.adelegue.mypictures.domains.picture

import java.nio.file.{Files, Paths}
import java.util.Date

import cats._
import com.adelegue.mypictures.domains.account.Accounts
import com.adelegue.mypictures.domains.account.Accounts.GetAccountByUsername
import com.adelegue.mypictures.domains.album.Albums
import com.adelegue.mypictures.domains.album.Albums.{Album, GetAlbum}
import com.adelegue.mypictures.domains.picture.Pictures.{Picture, PictureCreated, PictureDeleted, PictureUpdated}
import com.adelegue.mypictures.domains.picture.impl.{ImagesInterpreter, PicturesInterpreter}
import com.adelegue.mypictures.{AkkaPersistanceWithTmpFolder, PersistenceConfig}
import freek._
import org.specs2.mutable.Specification

import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, ExecutionContext, Future}
import scalaz.Success

/**
  * Created by adelegue on 24/06/2016.
  */
class PicturesSpec extends Specification {
  object Futured extends (cats.Id ~> Future) {
    def apply[A](a: cats.Id[A]) = Future.successful(a)
  }
  def await[T](f : Future[T]) = Await.result(f, 1 minute)

  "Picture store" should {

    "create read picture" in new AkkaPersistanceWithTmpFolder(PersistenceConfig()) {

      import cats.std.future._

      import ExecutionContext.Implicits.global

      val accountsInterpreterTest = new (Accounts.DSL ~> Id) {
        override def apply[A](fa: Accounts.DSL[A]): Id[A] = fa match {
          case GetAccountByUsername("") => None.asInstanceOf[Id[A]]
        }
      }

      val albumInterpreterTest = new (Albums.DSL ~> Id) {
        override def apply[A](fa: Albums.DSL[A]): Id[A] = fa match {
          case GetAlbum(id) => id match {
            case "12345" => Some(Album("12345", "12345", "titre", Some("desc"), new Date())).asInstanceOf[Id[A]]
            case _ => None.asInstanceOf[Id[A]]
          }
        }
      }

      val interpreter = PicturesInterpreter(system) :&: ImagesInterpreter(tmpFolder.getAbsolutePath) :&: (Futured compose albumInterpreterTest) :&: (Futured compose accountsInterpreterTest)

      val picture = Picture("12345", "filename", "jpg", "12345", false, Some("un titre"), Some("description"))
      val imageBytes = Files.readAllBytes(Paths.get("src/test/resources/images/tkd.jpg"))

      val res = await(Pictures.createPicture(picture, imageBytes).interpret(interpreter))

      res must beEqualTo(Success(PictureCreated(picture)))

      val img = await(Pictures.readImage(picture.id).interpret(interpreter))
      val thumbnail = await(Pictures.readThumbnail(picture.id).interpret(interpreter))

      val createdPicture = res.fold(f => throw new RuntimeException, p => p.picture)
      val expectedImg = Files.readAllBytes(Paths.get("src/test/resources/images/tkd-resized.jpg"))
      val expectedThumbnail = Files.readAllBytes(Paths.get("src/test/resources/images/tkd-thumbnail.jpg"))

      img must beSome(expectedImg)
      thumbnail must beSome(expectedThumbnail)

      val read = await(Pictures.getPicture("12345").interpret(interpreter))

      read must beSome(picture)
    }


    "update picture" in new AkkaPersistanceWithTmpFolder(PersistenceConfig()) {

      import cats.std.future._

      import ExecutionContext.Implicits.global

      val accountsInterpreterTest = new (Accounts.DSL ~> Id) {
        override def apply[A](fa: Accounts.DSL[A]): Id[A] = fa match {
          case GetAccountByUsername("") => None.asInstanceOf[Id[A]]
        }
      }

      val albumInterpreterTest = new (Albums.DSL ~> Id) {
        override def apply[A](fa: Albums.DSL[A]): Id[A] = fa match {
          case GetAlbum(id) => id match {
            case "12345" => Some(Album("12345", "12345", "titre", Some("desc"), new Date())).asInstanceOf[Id[A]]
            case _ => None.asInstanceOf[Id[A]]
          }
        }
      }

      val interpreter = PicturesInterpreter(system) :&: ImagesInterpreter(tmpFolder.getAbsolutePath) :&: (Futured compose albumInterpreterTest) :&: (Futured compose accountsInterpreterTest)

      val picture = Picture("12345", "filename", "jpg", "12345", false, Some("un titre"), Some("description"))
      val imageBytes = Files.readAllBytes(Paths.get("src/test/resources/images/tkd.jpg"))
      val pictureUpdate = picture.copy(title = Some("un titre updated"))
      val updated = for {
        _ <- Pictures.createPicture(picture, imageBytes)
        u <- Pictures.updatePicture(pictureUpdate)
      } yield u

      val res = await(updated.interpret(interpreter))

      res must beEqualTo(Success(PictureUpdated(pictureUpdate)))

      val img = await(Pictures.readImage(picture.id).interpret(interpreter))
      val thumbnail = await(Pictures.readThumbnail(picture.id).interpret(interpreter))

      val createdPicture = res.fold(f => throw new RuntimeException, p => p.picture)
      val expectedImg = Files.readAllBytes(Paths.get("src/test/resources/images/tkd-resized.jpg"))
      val expectedThumbnail = Files.readAllBytes(Paths.get("src/test/resources/images/tkd-thumbnail.jpg"))

      img must beSome(expectedImg)
      thumbnail must beSome(expectedThumbnail)

      val read = await(Pictures.getPicture("12345").interpret(interpreter))

      read must beSome(pictureUpdate)
    }


    "create and delete picture" in new AkkaPersistanceWithTmpFolder(PersistenceConfig()) {

      import cats.std.future._

      import ExecutionContext.Implicits.global

      val accountsInterpreterTest = new (Accounts.DSL ~> Id) {
        override def apply[A](fa: Accounts.DSL[A]): Id[A] = fa match {
          case GetAccountByUsername("") => None.asInstanceOf[Id[A]]
        }
      }

      val albumInterpreterTest = new (Albums.DSL ~> Id) {
        override def apply[A](fa: Albums.DSL[A]): Id[A] = fa match {
          case GetAlbum(id) => id match {
            case "12345" => Some(Album("12345", "12345", "titre", Some("desc"), new Date())).asInstanceOf[Id[A]]
            case _ => None.asInstanceOf[Id[A]]
          }
        }
      }

      val interpreter = PicturesInterpreter(system) :&: ImagesInterpreter(tmpFolder.getAbsolutePath) :&: (Futured compose albumInterpreterTest) :&: (Futured compose accountsInterpreterTest)

      val picture = Picture("12345", "filename", "jpg", "12345", false, Some("un titre"), Some("description"))
      val imageBytes = Files.readAllBytes(Paths.get("src/test/resources/images/tkd.jpg"))

      val res = await(Pictures.createPicture(picture, imageBytes).interpret(interpreter))

      res must beEqualTo(Success(PictureCreated(picture)))
      val read = await(Pictures.getPicture("12345").interpret(interpreter))
      read must beSome(picture)

      val deleted = await(Pictures.deletePicture("12345").interpret(interpreter))
      deleted must beEqualTo(Success(PictureDeleted("12345")))

      val readDeleted = await(Pictures.getPicture("12345").interpret(interpreter))
      readDeleted must beNone
    }
  }

}
