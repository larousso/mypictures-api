package services.album

import java.util.Date

import akka.actor.{ActorSystem, Props}
import akka.persistence.{PersistentActor, SnapshotOffer}
import akka.util.Timeout
import play.api.libs.json.Json
import services.Messages.{Cmd, Evt, Query}
import services.Persist
import services.account.Accounts
import services.album.Albums._
import services.picture.Pictures

import scala.concurrent.{ExecutionContext, Future}
import scalaz.Failure

/**
  * Created by adelegue on 28/05/2016.
  */


object Albums {

  object Api {
    case class Album(title: Albums.Title, description: Option[Albums.Description], date: Date = new Date(), pictureIds: List[Pictures.Id] = List.empty[Pictures.Id])
  }

  type Id = String
  type Title = String
  type Description = String

  case class Album(id: Id, username: Accounts.Username, title: Title, description: Option[Description], date: Date = new Date(), pictureIds: List[Pictures.Id] = List.empty[Pictures.Id])

  implicit val format = Json.format[Album]

  sealed trait AlbumCommand extends Cmd
  case class CreateAlbum(album: Album) extends AlbumCommand
  case class UpdateAlbum(album: Album) extends AlbumCommand
  case class DeleteAlbum(id: Id) extends AlbumCommand
  case class AddPicture(id: Id, pictureId: Pictures.Id) extends AlbumCommand
  case class RemovePicture(id: Id, pictureId: Pictures.Id) extends AlbumCommand

  sealed trait AlbumEvent extends Evt
  case class AlbumCreated(album: Album) extends AlbumEvent
  case class AlbumUpdated(album: Album) extends AlbumEvent
  case class AlbumDeleted(id: Id) extends AlbumEvent
  case class PictureAdded(id: Id, pictureId: Pictures.Id) extends AlbumEvent
  case class PictureRemoved(id: Id, pictureId: Pictures.Id) extends AlbumEvent

  sealed trait AlbumQuery extends Query
  case class GetAlbum(id: Id) extends AlbumQuery
  case class GetAlbumByUsername(username: Accounts.Username) extends AlbumQuery
  case object ListAlbums extends AlbumQuery

}

class Albums(accounts: Accounts)(implicit val system: ActorSystem) {

  import Albums._
  import akka.pattern.ask
  import services.validation.Validation._
  import system.dispatcher

  import scala.concurrent.duration.DurationDouble
  import scalaz.Scalaz._
  implicit val timeout = Timeout(5.second)

  val ref = system.actorOf(AlbumStoreActor.props, "Albums")

  def createAlbum(album: Album): Future[Result[AlbumCreated]] =
    for {
      validatedAlbum <- validateAlbumCreation(album)
      command <- validatedAlbum.fold(
        e => Future.successful(Failure(e)),
        s => (ref ? CreateAlbum(album)).mapTo[Result[AlbumCreated]]
      )
    } yield command

  def updateAlbum(album: Album): Future[Result[AlbumUpdated]] =
    for {
      validatedAlbum <- validateAlbumUpdate(album)
      command <- validatedAlbum.fold(
        e => Future.successful(Failure(e)),
        s => (ref ? UpdateAlbum(album)).mapTo[Result[AlbumUpdated]]
      )
    } yield command

  def addPicture(albumId: Albums.Id, pictureId: Pictures.Id): Future[Result[PictureAdded]] =
    (ref ? AddPicture(albumId, pictureId)).mapTo[Result[PictureAdded]]


  def removePicture(albumId: Albums.Id, pictureId: Pictures.Id): Future[Result[PictureRemoved]] =
    (ref ? RemovePicture(albumId, pictureId)).mapTo[Result[PictureRemoved]]


  def deleteAlbum(id: Albums.Id): Future[Result[AlbumDeleted]] =
    (ref ? DeleteAlbum(id)).mapTo[Result[AlbumDeleted]]

  def getAlbum(id: Albums.Id): Future[Option[Album]] =
    (ref ? GetAlbum(id)).mapTo[Option[Album]]

  def getAlbumByUsername(username: Accounts.Username): Future[List[Album]] =
    (ref ? GetAlbumByUsername(username)).mapTo[List[Album]]

  def listAll: Future[List[Album]] =
    (ref ? ListAlbums).mapTo[List[Album]]



  def validateAlbumCreation(album: Album): Future[Result[Album]] =
    for {
      username <- validateUsername(album)
      alreadyExists <- validateNotExists(album)
    } yield (username |@| alreadyExists) { (_, _) => album }


  def validateAlbumUpdate(album: Album): Future[Result[Album]] =
    for {
      username <- validateUsername(album)
      alreadyExists <- validateExists(album)
    } yield (username |@| alreadyExists) { (_, _) => album }

  def validateUsername(album: Album): Future[Result[Album]] =
    accounts.getAccountByUsername(album.username).map {
      case Some(a) => album.successNel
      case None => Error(s"L'utilisateur ${album.username} n'existe pas").failureNel
    }

  def validateNotExists(album: Album): Future[Result[Album]] =
    albumExists(album).map { exists => if (exists) Error("L'album existe déjà").failureNel else album.successNel }

  def validateExists(album: Album): Future[Result[Album]] =
    albumExists(album).map { exists => if (!exists) Error("L'album n'existe pas").failureNel else album.successNel }

  def albumExists(album: Album): Future[Boolean] =
    for {
      mayBe <- getAlbum(album.id)
    } yield mayBe.isDefined
}



object AlbumStoreActor {
  def props = Props(classOf[AlbumStoreActor])
}

class AlbumStoreActor extends PersistentActor {

  import scalaz.Scalaz._
  implicit val ec: ExecutionContext = context.system.dispatcher

  override def persistenceId: String = "albums"

  var state = AlbumState()

  def numEvents = state.size

  def updateState[E <: AlbumEvent](event: E) =
    state = state.updated(event)

  val receiveRecover: Receive = {
    case evt: AlbumEvent => updateState(evt)
    case SnapshotOffer(_, snapshot: AlbumState) => state = snapshot
  }

  val receiveCommand: Receive = {
    //Command
    case command: AlbumCommand =>
      command match {
        case CreateAlbum(album) =>
          self forward Persist(AlbumCreated(album))
        case UpdateAlbum(album) =>
          self forward Persist(AlbumUpdated(album))
        case DeleteAlbum(id) =>
          self forward Persist(AlbumDeleted(id))
        case AddPicture(id, pictureId) =>
          self forward Persist(PictureAdded(id, pictureId))
        case RemovePicture(id, pictureId) =>
          self forward Persist(PictureRemoved(id, pictureId))
      }
    case Persist(albumEvent: AlbumEvent) =>
      persist(albumEvent) { event =>
        updateState(event)
        context.system.eventStream.publish(event)
        sender() ! event.successNel
      }
    case query: AlbumQuery =>
      query match {
        //Queries
        case ListAlbums =>
          sender ! state.albums.values.toList
        case GetAlbum(id) =>
          sender ! state.albums.get(id)
        case GetAlbumByUsername(username) =>
          sender ! state.albums.values.filter(_.username == username)
      }
    case "snap" => saveSnapshot(state)
    case "print" => println(state)
  }
}

case class AlbumState(albums: Map[Albums.Id, Album] = Map.empty) {

  def updated(evt: AlbumEvent): AlbumState = evt match {
    case AlbumCreated(album) =>
      AlbumState(albums + (album.id -> album))
    case AlbumUpdated(album) =>
      AlbumState(albums + (album.id -> album))
    case AlbumDeleted(id) =>
      AlbumState(albums.filterKeys(_ != id))
    case PictureAdded(albumId, pictureId) =>
      AlbumState(albums.map {
        case (id, album) if id == albumId =>
          (id, album.copy(pictureIds = pictureId :: album.pictureIds))
        case any => any
      })
    case PictureRemoved(albumId, pictureId) =>
      AlbumState(albums.map {
        case (id, album) if id == albumId =>
          (id, album.copy(pictureIds = album.pictureIds.filterNot(_ == pictureId)))
        case any => any
      })
    case unknow =>
      println(s"$unknow")
      AlbumState(albums)
  }

  def size: Int = albums.size

  override def toString: String = albums.toString
}