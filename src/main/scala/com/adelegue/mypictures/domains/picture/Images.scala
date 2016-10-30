package com.adelegue.mypictures.domains.picture

import java.io.File
import java.nio.file.Files

import com.sksamuel.scrimage
import com.sksamuel.scrimage.nio.JpegWriter
import org.json4s.CustomSerializer
import org.json4s.JsonAST.JString

import scala.concurrent.{ExecutionContext, Future}
/**
  * Created by adelegue on 30/05/2016.
  */

object Images {
  type Id = String
  case class Image(id: Id, content: Array[Byte])
  case class Thumbnail(id: Id, content: Array[Byte])

  sealed trait Rotation
  case object Right extends Rotation
  case object Left extends Rotation

  class RotationSerializer extends CustomSerializer[Rotation](format => (
    {
      case JString(s) if s == "right" =>
        Right
      case JString(s) if s == "left" =>
        Left
    },
    {
      case Right =>
        JString("right")
      case Left =>
        JString("left")
  }))
}

class Images(basePath: String)(implicit val ec: ExecutionContext) {
  import Images._

  val baseFolder = new File(basePath)
  val baseThumbnailFolder = new File(basePath, "thumbnails")


  def readImage(id: Images.Id): Future[Option[Image]] = readAnImage(id, path(id))

  def createImage(id: Images.Id, content: Array[Byte]): Future[Image] =
    createAnImage(id, calcScale, path(id), content).map(res => Image(res._1, res._2))

  def rotateImage(id: Id, side: Rotation): Future[Image] =
    rotateAnImage(id, path(id), side)
      .map(res => Image(res._1, res._2))

  def deleteImage(id: Id): Future[Unit] = delete(path(id))

  def readThumbnail(id: Id): Future[Option[Thumbnail]] = Future {
    val thumbPath: File = thumbnailPath(id)
    if(thumbPath.exists()) {
      Some(Thumbnail(id, Files.readAllBytes(thumbPath.toPath)))
    } else {
      None
    }
  }

  def deleteThumbnail(id: Id): Future[Unit] = delete(thumbnailPath(id))

  def createThumbnail(id: Id, content: Array[Byte]): Future[Thumbnail] =
    createAnImage(id, calcThumbnailScale, thumbnailPath(id), content)
      .map(res => Thumbnail(res._1, res._2))

  def rotateThumbnail(id: Id, side: Rotation): Future[Thumbnail] =
    rotateAnImage(id, thumbnailPath(id), side)
      .map(res => Thumbnail(res._1, res._2))


  private def readAnImage(id: Images.Id, path: File) = Future {
    if (path.exists()) {
      Some(Image(id, Files.readAllBytes(path.toPath)))
    } else {
      None
    }
  }

  private def createAnImage(id: Images.Id, scale: scrimage.Image => Float, path: File, content: Array[Byte]): Future[(Id, Array[Byte])] =
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

  private def rotateAnImage(id: Images.Id, path: File, side: Rotation): Future[(Id, Array[Byte])] =
    Future {
      val image: scrimage.Image = side match {
        case Left => scrimage.Image.fromFile(path).rotateLeft
        case Right => scrimage.Image.fromFile(path).rotateRight
      }
      image.output(path)(JpegWriter())
      (id, image.bytes(JpegWriter()))
    }

  private def delete(path: File): Future[Unit] = Future {
    path.delete()
  }

  private def path(id: Images.Id): File = {
    if(!baseFolder.exists) {
      baseFolder.mkdirs
    }
    new File(baseFolder, id)
  }

  private def thumbnailPath(id: Images.Id): File = {
    if(!baseThumbnailFolder.exists) {
      baseThumbnailFolder.mkdirs
    }
    new File(baseThumbnailFolder, id)
  }

  private def calcScale(image: scrimage.Image) = {
    val (width, _) = image.dimensions
    1024f / width
  }

  private def calcThumbnailScale(image: scrimage.Image) = {
    val (width, _) = image.dimensions
    200f / width
  }
}