package com.adelegue.mypictures.domains.picture.impl

import java.io.File
import java.nio.file.{Files, Paths}

import cats.~>
import com.adelegue.mypictures.domains.picture.Images
import com.sksamuel.scrimage
import com.sksamuel.scrimage.nio.JpegWriter

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

/**
  * Created by adelegue on 18/06/2016.
  */
object ImagesInterpreter {
  def apply(basePath: String): ImagesInterpreter = new ImagesInterpreter(basePath)
}

class ImagesInterpreter(basePath: String) extends (Images.DSL ~> Future) {
  import Images._

  val baseFolder = new File(basePath)
  val baseThumbnailFolder = new File(basePath, "thumbnails")

  override def apply[A](fa: DSL[A]): Future[A] = fa match {
    case ReadImage(id) =>
      readImage(id, path(id)).asInstanceOf[Future[A]]
    case CreateImage(id, content) =>
      createImage(id, calcScale, path(id), content).map(res => Image(res._1, res._2)).asInstanceOf[Future[A]]
    case RotateImage(id, side) =>
      rotateImage(id, path(id), side)
        .map(res => Image(res._1, res._2))
        .asInstanceOf[Future[A]]
    case DeleteImage(id) =>
      delete(path(id)).asInstanceOf[Future[A]]
    case ReadThumbnail(id) =>
      Future {
        Thumbnail(id, Files.readAllBytes(thumbnailPath(id).toPath))
      }.asInstanceOf[Future[A]]
    case CreateThumbnail(id, content) =>
      createImage(id, calcThumbnailScale, thumbnailPath(id), content)
        .map(res => Thumbnail(res._1, res._2))
        .asInstanceOf[Future[A]]
    case RotateThumbnail(id, side) =>
      rotateImage(id, thumbnailPath(id), side)
        .map(res => Thumbnail(res._1, res._2))
        .asInstanceOf[Future[A]]
    case DeleteThumbnail(id) =>
      delete(thumbnailPath(id)).asInstanceOf[Future[A]]
  }

  def readImage(id: Images.Id, path: File) = Future {
    Image(id, Files.readAllBytes(path.toPath))
  }

  def createImage(id: Images.Id, scale: scrimage.Image => Float, path: File, content: Array[Byte]): Future[(Id, Array[Byte])] =
    Future {
      val image: scrimage.Image = scrimage.Image(content)
      val theScale = scale(image)
      val imageResized = if (theScale < 1) {
        image.scale(theScale)
      } else {
        image
      }
      imageResized.output(path)(JpegWriter())
      (id, imageResized.bytes(JpegWriter()))
    }

  def rotateImage(id: Images.Id, path: File, side: Rotation): Future[(Id, Array[Byte])] =
    Future {
      val image: scrimage.Image = side match {
        case Left => scrimage.Image.fromFile(path).rotateLeft
        case Right => scrimage.Image.fromFile(path).rotateRight
      }
      image.output(path)(JpegWriter())
      (id, image.bytes(JpegWriter()))
    }

  def delete(path: File): Future[Unit] = Future {
    path.delete()
  }

  def path(id: Images.Id): File = {
    if(!baseFolder.exists) {
      baseFolder.mkdirs
    }
    new File(baseFolder, id)
  }

  def thumbnailPath(id: Images.Id): File = {
    if(!baseThumbnailFolder.exists) {
      baseThumbnailFolder.mkdirs
    }
    new File(baseThumbnailFolder, id)
  }

  def calcScale(image: scrimage.Image) = {
    val (width, _) = image.dimensions
    1024f / width
  }
  def calcThumbnailScale(image: scrimage.Image) = {
    val (width, _) = image.dimensions
    200f / width
  }
}
