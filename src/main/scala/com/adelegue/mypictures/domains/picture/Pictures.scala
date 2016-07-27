package com.adelegue.mypictures.domains.picture

import cats.free.Free
import com.adelegue.mypictures.domains.Messages.{Cmd, Evt, Query}
import com.adelegue.mypictures.domains.album.Albums
import com.adelegue.mypictures.domains.picture.Images.Rotation
import com.adelegue.mypictures.validation.Validation._
import freek._

import scalaz.Failure

/**
  * Created by adelegue on 30/05/2016.
  */
object Pictures {

  type PRG = Pictures.DSL :|: Images.DSL :|: Albums.PRG

  def createPicture(picture: Picture, content: Array[Byte]): Free[PRG#Cop, Result[PictureCreated]] =
    for {
      ok <- validatePictureCreation(picture)
      created <- ok.fold(
        e => Free.pure[PRG#Cop, Result[PictureCreated]](Failure(e)),
        p => for {
          image <- Images.createImage(picture.id, content).expand[PRG]
          thumbnail <- Images.createThumbnail(picture.id, content).expand[PRG]
          created <- CreatePicture(picture).freek[PRG]
        } yield created
      )
    } yield created


  def updatePicture(picture: Picture): Free[PRG#Cop, Result[PictureUpdated]] =
    for {
      updated <- UpdatePicture(picture).freek[PRG]
    } yield updated

  def rotatePicture(id: Id, rotation: Rotation): Free[PRG#Cop, Option[Picture]] = {
    for {
      p <- getPicture(id)
      _ <- p match {
        case Some(_) =>
          for {
            _ <- Images.rotateImage(id, rotation).expand[PRG]
            _ <- Images.rotateThumbnail(id, rotation).expand[PRG]
          } yield Unit
        case None => Free.pure[PRG#Cop, Unit](Unit)
      }
    } yield p
  }

  def deletePicturesByAlbum(albumId: Albums.Id): Free[PRG#Cop, List[Result[PictureDeleted]]] = {
      import cats.implicits._
      for {
        pictures <- getPictureByAlbum(albumId)
        deletes <- pictures.traverseU { p => deletePicture(p.id) }
      } yield deletes
  }

  def deletePicture(id: Pictures.Id): Free[PRG#Cop, Result[PictureDeleted]] =
    for {
      _ <- Images.deleteImage(id).expand[PRG]
      _ <- Images.deleteThumbnail(id).expand[PRG]
      delete <- DeletePicture(id).freek[PRG]
    } yield delete

  def getPicture(id: Pictures.Id): Free[PRG#Cop, Option[Picture]] =
    for {
      picture <- GetPicture(id).freek[PRG]
    } yield picture

  def getPictureByAlbum(albumId: Albums.Id): Free[PRG#Cop, List[Picture]] = {
    for {
      pictures <- GetPictureByAlbum(albumId).freek[PRG]
    } yield pictures
  }

  def getThumbnailsByAlbum(albumId: Albums.Id): Free[PRG#Cop, List[Picture]] = {
    for {
      pictures <- GetPictureByAlbum(albumId).freek[PRG]
    } yield pictures
  }

  def listAll(): Free[PRG#Cop, List[Picture]] = {
    for {
      pictures <- ListPictures.freek[PRG]
    } yield pictures
  }

  def readImage(id: Pictures.Id): Free[PRG#Cop, Option[Array[Byte]]] =
    for {
      img <- Images.readImage(id).expand[PRG]
    } yield img.map(_.content)

  def readThumbnail(id: Pictures.Id): Free[PRG#Cop, Option[Array[Byte]]] =
    for {
      img <- Images.readThumbnail(id).expand[PRG]
    } yield img.map(_.content)

  def validatePictureCreation(picture: Picture): Free[PRG#Cop, Result[Picture]] = {
    import scalaz.Scalaz._
    for {
      albumExists <- validateAlbumExists(picture)
      pictureDoesntExist <- validatePictureDoesntExists(picture)
    } yield (albumExists |@| pictureDoesntExist) { (_, _) => picture}
  }

  def validateAlbumExists(picture: Picture): Free[PRG#Cop, Result[Picture]] = {
    import scalaz.Scalaz._
    for {
      album <- Albums.getAlbum(picture.album).expand[PRG]
    } yield album match {
      case Some(a) => picture.successNel
      case None => Error("L'album n'existe pas").failureNel
    }
  }

  def validatePictureExists(picture: Picture): Free[PRG#Cop, Result[Picture]] = {
    import scalaz.Scalaz._
    for {
      pict <- GetPicture(picture.id).freek[PRG]
    } yield pict match {
      case Some(e) => picture.successNel
      case None => Error("L'image n'existe pas").failureNel
    }
  }

  def validatePictureDoesntExists(picture: Picture): Free[PRG#Cop, Result[Picture]] = {
    import scalaz.Scalaz._
    for {
      pict <- GetPicture(picture.id).freek[PRG]
    } yield pict match {
      case Some(e) => Error("L'image existe déjà").failureNel
      case None => picture.successNel
    }
  }


  case class Picture(id: Pictures.Id, filename: Filename, `type`: Type, album: Albums.Id, preview: Boolean = false, title: Option[Title] = None, description: Option[String] = None)

  type Id = String
  type Title = String
  type Filename = String
  type Type = String


  sealed trait DSL[A]

  sealed trait PictureCommand extends Cmd

  case class CreatePicture(picture: Picture) extends PictureCommand with DSL[Result[PictureCreated]]

  case class UpdatePicture(picture: Picture) extends PictureCommand with DSL[Result[PictureUpdated]]

  case class DeletePicture(id: Pictures.Id) extends PictureCommand with DSL[Result[PictureDeleted]]

  sealed trait PictureEvent extends Evt

  case class PictureCreated(picture: Picture) extends PictureEvent

  case class PictureUpdated(picture: Picture) extends PictureEvent

  case class PictureDeleted(id: Pictures.Id) extends PictureEvent

  sealed trait PictureQuery extends Query

  case class GetPicture(id: Pictures.Id) extends PictureQuery with DSL[Option[Picture]]

  case class GetPictureByAlbum(albumId: Albums.Id) extends PictureQuery with DSL[List[Picture]]

  case object ListPictures extends PictureQuery with DSL[List[Picture]]

}
