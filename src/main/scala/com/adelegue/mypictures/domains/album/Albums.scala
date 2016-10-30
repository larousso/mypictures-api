package com.adelegue.mypictures.domains.album

import java.util.{Date, UUID}

import akka.actor.{ActorSystem, Props}
import akka.http.scaladsl.server._
import akka.persistence.{PersistentActor, SnapshotOffer}
import akka.util.Timeout
import com.adelegue.mypictures.domains.{Auth, Persist}
import com.adelegue.mypictures.domains.Messages.{Cmd, Evt, Query}
import com.adelegue.mypictures.domains.account.Accounts
import com.adelegue.mypictures.domains.album.Albums._
import com.adelegue.mypictures.domains.picture.Pictures
import com.adelegue.mypictures.validation.Validation._
import org.json4s.{Formats, Serialization}

import scala.concurrent.{ExecutionContext, Future}
import scalaz.Scalaz._
import scalaz.{Failure, Success}

/**
  * Created by adelegue on 28/05/2016.
  */


object Albums {

  object Api {

    case class Album(title: Albums.Title, description: Option[Albums.Description], date: Date = new Date(), pictureIds: List[Pictures.Id] = List.empty[Pictures.Id])

  }


  case class Api(auth: Auth, albums: Albums, pictures: Pictures)(implicit system: ActorSystem, serialization: Serialization, formats: Formats) {

    import system.dispatcher
    import akka.http.scaladsl.model._
    import akka.http.scaladsl.server.Directives._
    import de.heikoseeberger.akkahttpjson4s.Json4sSupport._

    def readAlbum(albumId: Albums.Id): Route = get {
      onSuccess(albums.getAlbum(albumId)) {
        case Some(a) => complete(a)
        case None => complete(StatusCodes.NotFound)
      }
    }

    def route(username: Accounts.Username)(subRoute: Albums.Id => Route): Route =
      auth.isAuthenticated {
        pathPrefix("albums") {
          pathEnd {
            get {
              onSuccess(albums.getAlbumByUsername(username)) { albums =>
                complete(albums)
              }
            } ~
              post {
                auth.hashRole(Accounts.Role.Admin) {
                  entity(as[Api.Album]) {
                    case Api.Album(title, description, date, pictureIds) =>
                      onSuccess(albums.createAlbum(Albums.Album(UUID.randomUUID.toString, username, title, description, date, pictureIds))) {
                        case Success(a) => complete(StatusCodes.Created -> a.album)
                        case Failure(e) => complete(StatusCodes.BadRequest -> e)
                      }
                  }
                }
              }
          } ~
            pathPrefix("[a-z0-9\\-]+".r) { albumId: Albums.Id =>
              pathEnd {
                readAlbum(albumId) ~
                  delete {
                    auth.hashRole(Accounts.Role.Admin) {
                      val res = for {
                        _ <- pictures.deletePicturesByAlbum(albumId)
                        delete <- albums.deleteAlbum(albumId)
                      } yield delete

                      onSuccess(res) {
                        case Success(a) =>
                          complete(StatusCodes.OK -> a)
                        case Failure(e) =>
                          complete(StatusCodes.BadRequest -> e)
                      }
                    }
                  } ~
                  put {
                    auth.hashRole(Accounts.Role.Admin) {
                      entity(as[Albums.Album]) { album =>
                        onSuccess(albums.updateAlbum(album)) {
                          case Success(a) =>
                            complete(StatusCodes.OK -> a.album)
                          case Failure(e) =>
                            complete(StatusCodes.BadRequest -> e)
                        }
                      }
                    }
                  }
              } ~
                subRoute(albumId)
            }
        }
      }
    }

  type Id = String
  type Title = String
  type Description = String

  case class Album(id: Id, username: Accounts.Username, title: Title, description: Option[Description], date: Date = new Date(), pictureIds: List[Pictures.Id] = List.empty[Pictures.Id])

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
  import system.dispatcher
  import com.adelegue.mypictures.validation.Validation._
  import scalaz.Scalaz._
  import akka.pattern.ask
  import scala.concurrent.duration.DurationDouble
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
    //Queries
    case ListAlbums =>
      sender ! state.albums.values.toList
    case GetAlbum(id) =>
      sender ! state.albums.get(id)
    case GetAlbumByUsername(username) =>
      sender ! state.albums.values.filter(_.username == username)
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