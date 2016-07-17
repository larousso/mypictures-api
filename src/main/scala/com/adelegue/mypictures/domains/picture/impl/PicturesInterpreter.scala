package com.adelegue.mypictures.domains.picture.impl

import akka.actor.{ActorSystem, Props}
import akka.persistence.{PersistentActor, SnapshotOffer}
import akka.util.Timeout
import cats._
import com.adelegue.mypictures.domains.Persist
import com.adelegue.mypictures.domains.picture.Pictures
import com.adelegue.mypictures.domains.picture.Pictures._
import scala.language.postfixOps
import scala.concurrent.duration.DurationInt
import scala.concurrent.Future
import com.adelegue.mypictures.validation.Validation._

/**
  * Created by adelegue on 30/05/2016.
  */

object PicturesInterpreter {
  def apply(system: ActorSystem): PicturesInterpreter = new PicturesInterpreter(system)
}

class PicturesInterpreter (system: ActorSystem) extends (Pictures.DSL ~> Future) {
  import akka.pattern.ask

  val ref = system.actorOf(PictureStoreActor.props)
  implicit val timeout = Timeout(2 second)

  override def apply[A](fa: DSL[A]): Future[A] = fa match {
    case c: CreatePicture => (ref ? c).mapTo[Result[PictureCreated]].asInstanceOf[Future[A]]
    case c: UpdatePicture => (ref ? c).mapTo[Result[PictureUpdated]].asInstanceOf[Future[A]]
    case c: DeletePicture => (ref ? c).mapTo[Result[PictureDeleted]].asInstanceOf[Future[A]]
    case q: GetPicture => (ref ? q).mapTo[Option[Picture]].asInstanceOf[Future[A]]
    case q: GetPictureByAlbum => (ref ? q).mapTo[List[Picture]].asInstanceOf[Future[A]]
    case ListPictures => (ref ? ListPictures).mapTo[List[Picture]].asInstanceOf[Future[A]]
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
