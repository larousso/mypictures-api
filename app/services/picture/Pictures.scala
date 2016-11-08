package services.picture

import akka.actor.{ActorSystem, Props}
import akka.persistence.{PersistentActor, SnapshotOffer}
import akka.util.Timeout
import cats.data.{NonEmptyList => Nel, Validated}
import play.api.libs.json.Json
import services.Messages.{Cmd, Evt, Query}
import services.Persist
import services.album.Albums
import services.picture.Images.Rotation
import services.picture.Pictures._
import services.validation.ValidatedT
import services.validation.Validation._

import scala.concurrent.Future

/**
  * Created by adelegue on 30/05/2016.
  */
object Pictures {

  object Api {

    case class RotationAction(rotation: Images.Rotation) extends Serializable
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

  import akka.pattern.ask
  import cats.Applicative
  import cats.implicits._
  import cats.data.Validated._
  import services.validation.ValidatedT._
  import services.validation.Validation._
  import system.dispatcher

  import scala.concurrent.duration.DurationDouble
  implicit val timeout = Timeout(5.second)

  val ref = system.actorOf(PictureStoreActor.props, "Pictures")

  def createPicture(picture: Picture, content: Array[Byte]): Future[Result[PictureCreated]] = {
    val res: ResultT[Future, PictureCreated] = for {
      ok <- ValidatedT(validatePictureCreation(picture))
      _ <- images.createImage(picture.id, content).liftT[ResultT]
      _ <- images.createThumbnail(picture.id, content).liftT[ResultT]
      _ <- ValidatedT(albums.addPicture(picture.album, picture.id))
      created <- ValidatedT((ref ? CreatePicture(picture)).mapTo[Result[PictureCreated]])
    } yield created
    res.value
  }

  def updatePicture(picture: Picture): Future[Result[PictureUpdated]] =
    (ref ? UpdatePicture(picture)).mapTo[Result[PictureUpdated]]

  def rotatePicture(id: Pictures.Id, rotation: Rotation): Future[Result[Picture]] = {
    for {
      p <- getPicture(id)
      _ <- images.rotateImage(id, rotation).map(Some.apply)
      _ <- images.rotateThumbnail(id, rotation).map(Some.apply)
    } yield Validated.fromOption(p, Nel.of(Error("Picture not found")))
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
    for {
      albumExists <- validateAlbumExists(picture)
      pictureDoesntExist <- validatePictureDoesntExists(picture)
    } yield Applicative[Result].map2(albumExists, pictureDoesntExist) { (_, _) => picture}
  }

  def validateAlbumExists(picture: Picture): Future[Result[Picture]] = {
    for {
      a <- albums.getAlbum(picture.album)
    } yield Validated.fromOption(a, Nel.of(Error("L'album n'existe pas"))).map(_ => picture)
  }

  def validatePictureExists(picture: Picture): Future[Result[Picture]] = {
    for {
      pict <- getPicture(picture.id)
    } yield Validated.fromOption(pict, Nel.of(Error("L'image n'existe pas")))
  }

  def validatePictureDoesntExists(picture: Picture): Future[Result[Picture]] = {
    for {
      pict <- getPicture(picture.id)
    } yield pict
        .map(_ => invalidNel(Error("L'image existe déjà")))
        .getOrElse(valid(picture))
  }
}



object PictureStoreActor {
  def props = Props(classOf[PictureStoreActor])
}

class PictureStoreActor extends PersistentActor {
  import cats.data.Validated._

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
        sender() ! valid(event)
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
