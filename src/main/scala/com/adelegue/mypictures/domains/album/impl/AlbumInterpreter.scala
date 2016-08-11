package com.adelegue.mypictures.domains.album.impl

import akka.actor.{ActorSystem, Props}
import akka.persistence.{PersistentActor, SnapshotOffer}
import akka.util.Timeout
import cats._
import com.adelegue.mypictures.domains.album.Albums.{ListAlbums, _}
import com.adelegue.mypictures.domains.album._
import com.adelegue.mypictures.domains.{Persist, UnknowEventException}
import com.adelegue.mypictures.validation.Validation._

import scala.concurrent.duration.DurationInt
import scala.concurrent.{ExecutionContext, Future}
import scalaz.Scalaz._
import scala.language.postfixOps

/**
  * Created by adelegue on 28/05/2016.
  */
object AlbumInterpreter {
  def apply(system: ActorSystem): AlbumInterpreter = new AlbumInterpreter(system)
}

class AlbumInterpreter(system: ActorSystem) extends (Albums.DSL ~> Future) {

  import akka.pattern.ask

  implicit val timeout = Timeout(5 second)
  val ref = system.actorOf(AlbumStoreActor.props)

  override def apply[A](fa: Albums.DSL[A]): Future[A] = fa match {
    case c: CreateAlbum =>
      (ref ? c).mapTo[Result[AlbumCreated]].asInstanceOf[Future[A]]
    case c: UpdateAlbum =>
      (ref ? c).mapTo[Result[AlbumUpdated]].asInstanceOf[Future[A]]
    case c: DeleteAlbum =>
      (ref ? c).mapTo[Result[AlbumDeleted]].asInstanceOf[Future[A]]
    case q: GetAlbum =>
      (ref ? q).mapTo[Option[Album]].asInstanceOf[Future[A]]
    case q: GetAlbumByUsername =>
      (ref ? q).mapTo[List[Album]].asInstanceOf[Future[A]]
    case ListAlbums =>
      (ref ? ListAlbums).mapTo[List[Album]].asInstanceOf[Future[A]]
    case c: AddPicture =>
      (ref ? c).mapTo[PictureAdded].asInstanceOf[Future[A]]
    case c: RemovePicture =>
      (ref ? c).mapTo[PictureRemoved].asInstanceOf[Future[A]]
  }
}


object AlbumStoreActor {
  def props = Props(classOf[AlbumStoreActor])
}

class AlbumStoreActor extends PersistentActor {

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
    //Commands
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
    case _ => throw new UnknowEventException()
  }

  def size: Int = albums.size

  override def toString: String = albums.toString
}