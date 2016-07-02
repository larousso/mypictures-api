package com.adelegue.mypictures.domains.picture

import cats.free.Free
import freek._
/**
  * Created by adelegue on 30/05/2016.
  */

object Images {

  type PRG = DSL :|: FXNil

  def readImage(id: Id): Free[PRG#Cop, Image] = ReadImage(id).freek[PRG]

  def createImage(id: Id, content: Array[Byte]): Free[PRG#Cop, Image] = CreateImage(id, content).freek[PRG]

  def rotateImage(id: Id, rotation: Rotation): Free[PRG#Cop, Image] = RotateImage(id, rotation).freek[PRG]

  def deleteImage(id: Id): Free[PRG#Cop, Unit] = DeleteImage(id).freek[PRG]

  def readThumbnail(id: Id): Free[PRG#Cop, Thumbnail] = ReadThumbnail(id).freek[PRG]

  def createThumbnail(id: Id, content: Array[Byte]): Free[PRG#Cop, Thumbnail] = CreateThumbnail(id, content).freek[PRG]

  def rotateThumbnail(id: Id, rotation: Rotation): Free[PRG#Cop, Thumbnail] = RotateThumbnail(id, rotation).freek[PRG]

  def deleteThumbnail(id: Id): Free[PRG#Cop, Unit] = DeleteThumbnail(id).freek[PRG]

  sealed trait DSL[A]
  type Id = String
  case class Image(id: Id, content: Array[Byte])
  case class Thumbnail(id: Id, content: Array[Byte])
  case class ReadImage(id: Id) extends DSL[Image]
  case class ReadThumbnail(id: Id) extends DSL[Thumbnail]
  case class DeleteImage(id: Id) extends DSL[Unit]
  case class DeleteThumbnail(id: Id) extends DSL[Unit]
  case class CreateImage(id: Id, content: Array[Byte]) extends DSL[Image]
  case class CreateThumbnail(id: Id, content: Array[Byte]) extends DSL[Thumbnail]
  case class RotateImage(id: Id, side: Rotation) extends DSL[Image]
  case class RotateThumbnail(id: Id, side: Rotation) extends DSL[Thumbnail]

  sealed trait Rotation
  case object Right extends Rotation
  case object Left extends Rotation
}
