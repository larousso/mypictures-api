package services.comment

import java.util.{Date, UUID}

import akka.actor.{ActorSystem, Props}
import akka.http.scaladsl.server.Route
import akka.persistence.{PersistentActor, SnapshotOffer}
import akka.util.Timeout
import services.Messages.{Cmd, Evt, Query}
import services.comment.Comments._
import services.picture.Pictures
import services.{Persist, UnknowEventException, validation}
import play.api.libs.json.Json
import services.validation.ValidatedT

import scala.concurrent.{ExecutionContext, Future}

/**
  * Created by adelegue on 17/07/2016.
  */
object Comments {

  object Api {
    case class Comment(name: String, comment: String, date: Date = new Date())
    implicit val format = Json.format[Comment]

  }

  type Id = String

  case class Comment(id: Comments.Id, pictureId: Pictures.Id, name: String, comment: String, date: Date)

  implicit val format = Json.format[Comment]

  sealed trait CommentCmd extends Cmd
  case class CreateComment(comment: Comment) extends CommentCmd
  case class UpdateComment(comment: Comment) extends CommentCmd
  case class DeleteComment(id: Comments.Id) extends CommentCmd

  sealed trait CommentEvent extends Evt
  case class CommentCreated(comment: Comment) extends CommentEvent
  case class CommentUpdated(comment: Comment) extends CommentEvent
  case class CommentDeleted(id: Comments.Id) extends CommentEvent

  sealed trait CommentQuery extends Query
  case class GetComment(id: Comments.Id) extends CommentQuery
  case class ListCommentsByPicture(pictureId: Pictures.Id) extends CommentQuery

}

class Comments(pictures: Pictures)(implicit val system: ActorSystem) {

  import system.dispatcher
  import akka.pattern.ask
  import services.validation.Validation._
  import cats.Applicative
  import cats.data.Validated._
  import cats.implicits._


  import scala.concurrent.duration.DurationDouble
  implicit val timeout = Timeout(5.second)

  val ref = system.actorOf(CommentStoreActor.props, "Comments")

  def getComment(id: Comments.Id): Future[Option[Comment]] =
      (ref ? GetComment(id)).mapTo[Option[Comment]]

  def listCommentsByPicture(pictureId: Pictures.Id): Future[List[Comment]] =
    (ref ? ListCommentsByPicture(pictureId)).mapTo[List[Comment]]


  def createComment(comment: Comment): Future[Result[CommentCreated]] = {
    (for {
      validated <- ValidatedT(validatePictureExists(comment))
      added <- ValidatedT((ref ? CreateComment(comment)).mapTo[Result[CommentCreated]])
    } yield added).value
  }

  def updateComment(comment: Comment): Future[Result[CommentUpdated]] = {
    (for {
      validated <- ValidatedT(validateCommentUpdate(comment))
      added <- ValidatedT((ref ? UpdateComment(comment)).mapTo[Result[CommentUpdated]])
    } yield added).value
  }

  def deleteComment(id: Comments.Id): Future[Result[CommentDeleted]] =
    (ref ? DeleteComment(id)).mapTo[Result[CommentDeleted]]


  def validateCommentExists(comment: Comment) : Future[Result[Comment]] = {
    for {
      mayBe <- getComment(comment.id)
    } yield mayBe.map(valid).getOrElse(invalidNel(Error("Le commentaire n'existe pas")))
  }

  def validateCommentUpdate(comment: Comment) : Future[Result[Comment]] = {
    for {
      exists <- validateCommentExists(comment)
      pExists <- validatePictureExists(comment)
    } yield Applicative[Result].map2(exists, pExists) { (_, _) => comment }
  }

  def validatePictureExists(comment: Comment): Future[Result[Comment]] = {
    for {
      picture <- pictures.getPicture(comment.pictureId)
    } yield picture.map(_ => valid(comment)).getOrElse(invalidNel(Error("L'image n'existe pas")))
  }
}



object CommentStoreActor {
  def props = Props(classOf[CommentStoreActor])
}

class CommentStoreActor extends PersistentActor {
  import cats.data.Validated._
  implicit val ec: ExecutionContext = context.system.dispatcher

  override def persistenceId: String = "comments"

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
        sender() ! valid(event)
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