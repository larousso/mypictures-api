package com.adelegue.mypictures.domains.comment

import java.util.{Date, UUID}

import akka.actor.{ActorSystem, Props}
import akka.http.scaladsl.server.Route
import akka.persistence.{PersistentActor, SnapshotOffer}
import akka.util.Timeout
import com.adelegue.mypictures.domains.Messages.{Cmd, Evt, Query}
import com.adelegue.mypictures.domains.comment.Comments._
import com.adelegue.mypictures.domains.picture.Pictures
import com.adelegue.mypictures.domains.{Persist, UnknowEventException}
import org.json4s.{Formats, Serialization}

import scala.concurrent.{ExecutionContext, Future}
import scalaz.{Failure, Success}

/**
  * Created by adelegue on 17/07/2016.
  */
object Comments {

  object Api {
    case class Comment(name: String, comment: String, date: Date = new Date())
  }


  case class Api(comments: Comments)(implicit system: ActorSystem, serialization: Serialization, formats: Formats) {
    import akka.http.scaladsl.model._
    import akka.http.scaladsl.server.Directives._
    import de.heikoseeberger.akkahttpjson4s.Json4sSupport._


    def route(pictureId: Pictures.Id) : Route  =
    pathPrefix("comments") {
      pathEnd {
        get {
          onSuccess(comments.listCommentsByPicture(pictureId)) {
            case l => complete(l)
          }
        } ~
          post {
            entity(as[Api.Comment]) { comment =>
              onSuccess(comments.createComment(Comments.Comment(UUID.randomUUID.toString, pictureId, comment.name, comment.comment, comment.date))) {
                case Success(c) => complete(StatusCodes.OK -> c.comment)
                case Failure(e) => complete(StatusCodes.BadRequest -> e)
              }
            }
          }
      } ~
        path("[a-z0-9\\-]+".r) { commentId =>
          put {
            entity(as[Comments.Comment]) { comment =>
              onSuccess(comments.updateComment(comment)) {
                case Success(c) => complete(StatusCodes.OK -> c.comment)
                case Failure(e) => complete(StatusCodes.BadRequest -> e)
              }
            }
          } ~
            delete {
              onSuccess(comments.deleteComment(commentId)) {
                case Success(c) => complete(StatusCodes.OK -> c)
                case Failure(e) => complete(StatusCodes.BadRequest -> e)
              }
            }
        }
    }
  }

  type Id = String

  case class Comment(id: Comments.Id, pictureId: Pictures.Id, name: String, comment: String, date: Date)

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
  import com.adelegue.mypictures.validation.Validation._

  import scala.concurrent.duration.DurationDouble
  implicit val timeout = Timeout(5.second)

  val ref = system.actorOf(CommentStoreActor.props, "Comments")

  def getComment(id: Comments.Id): Future[Option[Comment]] =
      (ref ? GetComment(id)).mapTo[Option[Comment]]

  def listCommentsByPicture(pictureId: Pictures.Id): Future[List[Comment]] =
    (ref ? ListCommentsByPicture(pictureId)).mapTo[List[Comment]]


  def createComment(comment: Comment): Future[Result[CommentCreated]] = {
    for {
      validated <- validatePictureExists(comment)
      added <- validated.fold(
        e => Future.successful(Failure(e)),
        c => for {
          a <- (ref ? CreateComment(comment)).mapTo[Result[CommentCreated]]
        } yield a
      )
    } yield added
  }

  def updateComment(comment: Comment): Future[Result[CommentUpdated]] = {
    for {
      validated <- validateCommentUpdate(comment)
      added <- validated.fold(
        e => Future.successful(Failure(e)),
        c => for {
          a <- (ref ? UpdateComment(comment)).mapTo[Result[CommentUpdated]]
        } yield a
      )
    } yield added
  }

  def deleteComment(id: Comments.Id): Future[Result[CommentDeleted]] =
    (ref ? DeleteComment(id)).mapTo[Result[CommentDeleted]]


  def validateCommentExists(comment: Comment) : Future[Result[Comment]] = {
    import scalaz.Scalaz._
    for {
      mayBe <- getComment(comment.id)
    } yield mayBe match {
      case Some(c) => c.successNel
      case None => Error("Le commentaire n'existe pas").failureNel
    }
  }

  def validateCommentUpdate(comment: Comment) : Future[Result[Comment]] = {
    import scalaz.Scalaz._
    for {
      exists <- validateCommentExists(comment)
      pExists <- validatePictureExists(comment)
    } yield (exists |@| pExists) { (_, _) => comment }
  }

  def validatePictureExists(comment: Comment): Future[Result[Comment]] = {
    import scalaz.Scalaz._
    for {
      picture <- pictures.getPicture(comment.pictureId)
    } yield picture match {
      case Some(a) => comment.successNel
      case None => Error("L'image n'existe pas").failureNel
    }
  }
}



object CommentStoreActor {
  def props = Props(classOf[CommentStoreActor])
}

class CommentStoreActor extends PersistentActor {
  import scalaz.Scalaz._
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