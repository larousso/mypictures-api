package com.adelegue.mypictures.domains.picture

import java.nio.file.{Files, Paths}

import com.adelegue.mypictures.{TmpFolderSpec2}
import com.adelegue.mypictures.domains.picture.impl.ImagesInterpreter
import freek._
import org.specs2.mutable.Specification

import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, ExecutionContext, Future}

/**
  * Created by adelegue on 18/06/2016.
  */
class ImagesSpec extends Specification {

  def await[T](f : Future[T]) = Await.result(f, 1.minute)

  "Images" should {

    "create image with scale" in new TmpFolderSpec2 () {

      import cats.std.future._
      import ExecutionContext.Implicits.global

      val imageBytes = Files.readAllBytes(Paths.get("src/test/resources/images/tkd.jpg"))

      val interpreter = ImagesInterpreter(tmpFolder.getAbsolutePath)
      val img = await(Images.createImage("12345", imageBytes).interpret(interpreter))

      val imageResizedBytes = Files.readAllBytes(Paths.get("src/test/resources/images/tkd-resized.jpg"))

      (img.content.length / 1024) must lessThan(120)
      img.content must equalTo(imageResizedBytes)


    }

    "create image and read" in new TmpFolderSpec2 () {

      import cats.std.future._
      import ExecutionContext.Implicits.global

      val imageBytes = Files.readAllBytes(Paths.get("src/test/resources/images/tkd.jpg"))

      val interpreter = ImagesInterpreter(tmpFolder.getAbsolutePath)
      val created = await(Images.createImage("12345", imageBytes).interpret(interpreter))
      val read = await(Images.readImage("12345").interpret(interpreter))

      created.content must equalTo(read.get.content)
    }

    "rotate image" in new TmpFolderSpec2 () {

      import cats.std.future._
      import ExecutionContext.Implicits.global

      val imageBytes = Files.readAllBytes(Paths.get("src/test/resources/images/tkd.jpg"))

      val interpreter = ImagesInterpreter(tmpFolder.getAbsolutePath)

      val rotation = for {
        _ <- Images.createImage("23456", imageBytes)
        rotate <- Images.rotateImage("23456", Images.Left)
      } yield rotate

      val img = await(rotation.interpret(interpreter))

      val expected = Files.readAllBytes(Paths.get("src/test/resources/images/tkd-rotate-left.jpg"))
      (img.content.length / 1024) must lessThan(120)
      img.content must beEqualTo(expected)
    }

    "create thumbnail with scale" in new TmpFolderSpec2 () {

      import cats.std.future._
      import ExecutionContext.Implicits.global

      val imageBytes = Files.readAllBytes(Paths.get("src/test/resources/images/tkd.jpg"))

      val interpreter = ImagesInterpreter(tmpFolder.getAbsolutePath)
      val img = await(Images.createThumbnail("54321", imageBytes).interpret(interpreter))

      val imageResizedBytes = Files.readAllBytes(Paths.get("src/test/resources/images/tkd-thumbnail.jpg"))

      (img.content.length / 1024) must lessThan(10)
      img.content must equalTo(imageResizedBytes)
    }

    "rotate thumbnail" in new TmpFolderSpec2 () {

      import cats.std.future._
      import ExecutionContext.Implicits.global

      val imageBytes = Files.readAllBytes(Paths.get("src/test/resources/images/tkd.jpg"))

      val interpreter = ImagesInterpreter(tmpFolder.getAbsolutePath)

      val rotation = for {
        _ <- Images.createThumbnail("65432", imageBytes)
        rotate <- Images.rotateThumbnail("65432", Images.Left)
      } yield rotate

      val img = await(rotation.interpret(interpreter))

      val expected = Files.readAllBytes(Paths.get("src/test/resources/images/tkd-thumbnail-rotate-left.jpg"))
      (img.content.length / 1024) must lessThan(10)
      img.content must beEqualTo(expected)
    }

  }
}
