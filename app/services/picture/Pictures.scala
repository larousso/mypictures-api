package services.picture

import akka.actor.{ActorSystem, Props}
import akka.http.scaladsl.server._
import akka.persistence.{PersistentActor, SnapshotOffer}
import akka.stream.Materializer
import akka.util.{ByteString, Timeout}
import services.{Persist}
import services.Messages.{Cmd, Evt, Query}
import services.account.Accounts.Role.Admin
import services.album.Albums
import services.picture.Images.Rotation
import services.picture.Pictures._
import org.json4s.{Formats, Serialization}
import play.api.libs.json.Json

import scala.concurrent.Future
import scalaz.{Failure, Success}

/**
  * Created by adelegue on 30/05/2016.
  */
object Pictures {

  object Api {

    case class RotationAction(rotation: Images.Rotation) extends Serializable
    import services.picture.Images._
    implicit val format = Json.format[RotationAction]
  }

  case class Picture(id: Pictures.Id, filename: Filename, `type`: Type, album: Albums.Id, preview: Boolean = false, title: Option[Title] = None, description: Option[String] = None)

  implicit val format = Json.format[Picture]

  type Id = String
  type Title = String
  type Filename = String
  type Type = String

  sealed trait PictureCommand extends Cmd

  case class CreatePicture(picture: Picture) extends PictureCommand

  case class UpdatePicture(picture: Picture) extends PictureCommand

  case class DeletePicture(id: Pictures.Id) extends PictureCommand

  sealed trait PictureEvent extends Evt

  case class PictureCreated(picture: Picture) extends PictureEvent

  case class PictureUpdated(picture: Picture) extends PictureEvent

  case class PictureDeleted(id: Pictures.Id) extends PictureEvent

  sealed trait PictureQuery extends Query

  case class GetPicture(id: Pictures.Id) extends PictureQuery

  case class GetPictureByAlbum(albumId: Albums.Id) extends PictureQuery

  case object ListPictures extends PictureQuery

}


class Pictures(albums: Albums, images: Images)(implicit val system: ActorSystem) {

  import system.dispatcher
  import akka.pattern.ask
  import services.validation.Validation._

  import scala.concurrent.duration.DurationDouble
  implicit val timeout = Timeout(5.second)

  val ref = system.actorOf(PictureStoreActor.props, "Pictures")

  def createPicture(picture: Picture, content: Array[Byte]): Future[Result[PictureCreated]] =
    for {
      ok <- validatePictureCreation(picture)
      created <- ok.fold(
        e => Future.successful(Failure(e)),
        p => for {
          _ <- images.createImage(picture.id, content)
          _ <- images.createThumbnail(picture.id, content)
          _ <- albums.addPicture(picture.album, picture.id)
          created <- (ref ? CreatePicture(picture)).mapTo[Result[PictureCreated]]
        } yield created
      )
    } yield created


  def updatePicture(picture: Picture): Future[Result[PictureUpdated]] =
    (ref ? UpdatePicture(picture)).mapTo[Result[PictureUpdated]]

  def rotatePicture(id: Pictures.Id, rotation: Rotation): Future[Option[Picture]] = {
    for {
      p <- getPicture(id)
      _ <- p match {
        case Some(_) =>
          for {
            _ <- images.rotateImage(id, rotation)
            _ <- images.rotateThumbnail(id, rotation)
          } yield Unit
        case None => Future.successful(Unit)
      }
    } yield p
  }

  def deletePicturesByAlbum(albumId: Albums.Id): Future[List[Result[PictureDeleted]]] = {
    import cats.implicits._
    for {
      pictures <- getPictureByAlbum(albumId)
      deletes <- pictures.traverseU { p => deletePicture(albumId, p.id) }
    } yield deletes
  }

  def deletePicture(albumId: Albums.Id, id: Pictures.Id): Future[Result[PictureDeleted]] =
    for {
      _ <- images.deleteImage(id)
      _ <- images.deleteThumbnail(id)
      _ <- albums.removePicture(albumId, id)
      delete <- (ref ? DeletePicture(id)).mapTo[Result[PictureDeleted]]
    } yield delete

  def getPicture(id: Pictures.Id): Future[Option[Picture]] =
    (ref ? GetPicture(id)).mapTo[Option[Picture]]

  def getPictureByAlbum(albumId: Albums.Id): Future[List[Picture]] =
    (ref ? GetPictureByAlbum(albumId)).mapTo[List[Picture]]


  def getThumbnailsByAlbum(albumId: Albums.Id): Future[List[Picture]] =
    (ref ? GetPictureByAlbum(albumId)).mapTo[List[Picture]]


  def listAll(): Future[List[Picture]] =
    (ref ? ListPictures).mapTo[List[Picture]]


  def readImage(id: Pictures.Id): Future[Option[Array[Byte]]] =
    for {
      img <- images.readImage(id)
    } yield img.map(_.content)

  def readThumbnail(id: Pictures.Id): Future[Option[Array[Byte]]] =
    for {
      img <- images.readThumbnail(id)
    } yield img.map(_.content)

  def validatePictureCreation(picture: Picture): Future[Result[Picture]] = {
    import scalaz.Scalaz._
    for {
      albumExists <- validateAlbumExists(picture)
      pictureDoesntExist <- validatePictureDoesntExists(picture)
    } yield (albumExists |@| pictureDoesntExist) { (_, _) => picture}
  }

  def validateAlbumExists(picture: Picture): Future[Result[Picture]] = {
    import scalaz.Scalaz._
    for {
      album <- albums.getAlbum(picture.album)
    } yield album match {
      case Some(a) => picture.successNel
      case None => Error("L'album n'existe pas").failureNel
    }
  }

  def validatePictureExists(picture: Picture): Future[Result[Picture]] = {
    import scalaz.Scalaz._
    for {
      pict <- getPicture(picture.id)
    } yield pict match {
      case Some(e) => picture.successNel
      case None => Error("L'image n'existe pas").failureNel
    }
  }

  def validatePictureDoesntExists(picture: Picture): Future[Result[Picture]] = {
    import scalaz.Scalaz._
    for {
      pict <- getPicture(picture.id)
    } yield pict match {
      case Some(e) => Error("L'image existe déjà").failureNel
      case None => picture.successNel
    }
  }
}



object PictureStoreActor {
  def props = Props(classOf[PictureStoreActor])
}

class PictureStoreActor extends PersistentActor {
  import scalaz.Scalaz._

  override def persistenceId: String = "pictures"

  var state = PictureState()

  def numEvents = state.size

  def updateState[E <: PictureEvent](event: E) = {
    state = state.updated(event)
  }

  val receiveRecover: Receive = {
    case evt: PictureEvent => updateState(evt)
    case SnapshotOffer(_, snapshot: PictureState) => state = snapshot
  }

  val receiveCommand: Receive = {
    case CreatePicture(picture) =>
      self forward  Persist(PictureCreated(picture))
    case UpdatePicture(picture) =>
      self forward  Persist(PictureUpdated(picture))
    case DeletePicture(id) =>
      self forward  Persist(PictureDeleted(id))
    case Persist(pictureEvent: PictureEvent) =>
      persist(pictureEvent) { event =>
        updateState(event)
        context.system.eventStream.publish(event)
        sender() ! event.successNel
      }
    case ListPictures =>
      sender ! state.pictures.values.toList
    case GetPicture(id) =>
      sender ! state.pictures.get(id)
    case GetPictureByAlbum(id) =>
      sender ! state.pictures.values.toList.filter(p => p.album == id)
    case "snap" => saveSnapshot(state)
    case "print" => println(state)
  }
}

case class PictureState(pictures: Map[Pictures.Id, Picture] = Map.empty) {
  def updated(evt: PictureEvent): PictureState = evt match {
    case PictureCreated(picture) =>
      PictureState(pictures + (picture.id -> picture))
    case PictureUpdated(picture) =>
      PictureState(pictures + (picture.id -> picture))
    case PictureDeleted(id) =>
      PictureState(pictures.filterKeys(_ != id))
    case _ => this
  }

  def size: Int = pictures.size

  override def toString: String = pictures.toString
}
