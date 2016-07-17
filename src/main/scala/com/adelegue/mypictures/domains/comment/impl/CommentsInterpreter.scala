package com.adelegue.mypictures.domains.comment.impl

import akka.actor.{ActorSystem, Props}
import akka.persistence.{PersistentActor, SnapshotOffer}
import akka.util.Timeout
import cats._
import com.adelegue.mypictures.domains.comment.Comments._
import com.adelegue.mypictures.domains.comment._
import com.adelegue.mypictures.domains.{Persist, UnknowEventException}
import com.adelegue.mypictures.validation.Validation._

import scala.concurrent.duration.DurationInt
import scala.concurrent.{ExecutionContext, Future}
import scalaz.Scalaz._
import scala.language.postfixOps

/**
  * Created by adelegue on 28/05/2016.
  */
object CommentInterpreter {
  def apply(system: ActorSystem): CommentInterpreter = new CommentInterpreter(system)
}

class CommentInterpreter(system: ActorSystem) extends (Comments.DSL ~> Future) {

  import akka.pattern.ask

  implicit val timeout = Timeout(5 second)
  val ref = system.actorOf(CommentStoreActor.props)

  override def apply[A](fa: Comments.DSL[A]): Future[A] = fa match {
    case c: CreateComment =>
      (ref ? c).mapTo[Result[CommentCreated]].asInstanceOf[Future[A]]
    case c: UpdateComment =>
      (ref ? c).mapTo[Result[CommentUpdated]].asInstanceOf[Future[A]]
    case c: DeleteComment =>
      (ref ? c).mapTo[Result[CommentDeleted]].asInstanceOf[Future[A]]
    case q: GetComment =>
      (ref ? q).mapTo[Option[Comment]].asInstanceOf[Future[A]]
    case q: ListCommentsByPicture =>
      (ref ? q).mapTo[List[Comment]].asInstanceOf[Future[A]]
  }
}


object CommentStoreActor {
  def props = Props(classOf[CommentStoreActor])
}

class CommentStoreActor extends PersistentActor {

  implicit val ec: ExecutionContext = context.system.dispatcher

  override def persistenceId: String = "albums"

  var state = CommentState()

  def numEvents = state.size

  def updateState[E <: CommentEvent](event: E) =
    state = state.updated(event)

  val receiveRecover: Receive = {
    case evt: CommentEvent => updateState(evt)
    case SnapshotOffer(_, snapshot: CommentState) => state = snapshot
  }

  val receiveCommand: Receive = {
    //Commands
    case CreateComment(album) =>
      self forward Persist(CommentCreated(album))
    case UpdateComment(album) =>
      self forward Persist(CommentUpdated(album))
    case DeleteComment(id) =>
      self forward Persist(CommentDeleted(id))
    case Persist(albumEvent: CommentEvent) =>
      persist(albumEvent) { event =>
        updateState(event)
        context.system.eventStream.publish(event)
        sender() ! event.successNel
      }
    //Queries
    case GetComment(id) =>
      sender ! state.comments.get(id)
    case ListCommentsByPicture(pictureId) =>
      sender ! state.comments.values.filter(_.pictureId == pictureId)
    case "snap" => saveSnapshot(state)
    case "print" => println(state)
  }
}

case class CommentState(comments: Map[Comments.Id, Comment] = Map.empty) {

  def updated(evt: CommentEvent): CommentState = evt match {
    case CommentCreated(album) =>
      CommentState(comments + (album.id -> album))
    case CommentUpdated(album) =>
      CommentState(comments + (album.id -> album))
    case CommentDeleted(id) =>
      CommentState(comments.filterKeys(_ != id))
    case _ => throw new UnknowEventException()
  }

  def size: Int = comments.size

  override def toString: String = comments.toString
}