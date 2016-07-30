package com.adelegue.mypictures.domains.comment

import java.util.Date

import cats.free.Free
import com.adelegue.mypictures.validation.Validation._
import com.adelegue.mypictures.domains.picture.Pictures
import com.adelegue.mypictures.domains.Messages.{Cmd, Evt, Query}
import com.adelegue.mypictures.domains.album.Albums
import com.adelegue.mypictures.domains.comment.Comments._
import freek._

import scalaz.Failure

/**
  * Created by adelegue on 17/07/2016.
  */
object Comments {

  type PRG = Comments.DSL :|: Pictures.PRG
  val PRG = Program[PRG]
  
  
  def getComment(id: Comments.Id): Free[PRG.Cop, Option[Comment]] = {
    for {
      comment <- GetComment(id).freek[PRG]
    } yield comment
  }

  def listCommentsByPicture(pictureId: Pictures.Id): Free[PRG.Cop, List[Comment]] = {
    for {
      comments <- ListCommentsByPicture(pictureId).freek[PRG]
    } yield comments
  }

  def createComment(comment: Comment): Free[PRG.Cop, Result[CommentCreated]] = {
    for {
      validated <- validatePictureExists(comment)
      added <- validated.fold(
        e => Free.pure[PRG.Cop, Result[CommentCreated]](Failure(e)),
        c => for {
          a <- CreateComment(comment).freek[PRG]
        } yield a
      )
    } yield added
  }

  def updateComment(comment: Comment): Free[PRG.Cop, Result[CommentUpdated]] = {
    for {
      validated <- validateCommentUpdate(comment)
      added <- validated.fold(
        e => Free.pure[PRG.Cop, Result[CommentUpdated]](Failure(e)),
        c => for {
          a <- UpdateComment(comment).freek[PRG]
        } yield a
      )
    } yield added
  }

  def deleteComment(id: Comments.Id): Free[PRG.Cop, Result[CommentDeleted]] = {
    for {
      comment <- DeleteComment(id).freek[PRG]
    } yield comment
  }

  def validateCommentExists(comment: Comment) : Free[PRG.Cop, Result[Comment]] = {
    import scalaz.Scalaz._
    for {
      mayBe <- getComment(comment.id)
    } yield mayBe match {
      case Some(c) => c.successNel
      case None => Error("Le commentaire n'existe pas").failureNel
    }
  }

  def validateCommentUpdate(comment: Comment) : Free[PRG.Cop, Result[Comment]] = {
    import scalaz.Scalaz._
    for {
      exists <- validateCommentExists(comment)
      pExists <- validatePictureExists(comment)
    } yield (exists |@| pExists) { (_, _) => comment }
  }

  def validatePictureExists(comment: Comment): Free[PRG.Cop, Result[Comment]] = {
    import scalaz.Scalaz._
    for {
      picture <- Pictures.getPicture(comment.pictureId).expand[PRG]
    } yield picture match {
      case Some(a) => comment.successNel
      case None => Error("L'image n'existe pas").failureNel
    }
  }


  type Id = String

  case class Comment(id: Comments.Id, pictureId: Pictures.Id, name: String, comment: String, date: Date)
  sealed trait DSL[A]

  sealed trait CommentCmd extends Cmd
  case class CreateComment(comment: Comment) extends CommentCmd with DSL[Result[CommentCreated]]
  case class UpdateComment(comment: Comment) extends CommentCmd with DSL[Result[CommentUpdated]]
  case class DeleteComment(id: Comments.Id) extends CommentCmd with DSL[Result[CommentDeleted]]

  sealed trait CommentEvent extends Evt
  case class CommentCreated(comment: Comment) extends CommentEvent
  case class CommentUpdated(comment: Comment) extends CommentEvent
  case class CommentDeleted(id: Comments.Id) extends CommentEvent

  sealed trait CommentQuery extends Query
  case class GetComment(id: Comments.Id) extends CommentQuery with DSL[Option[Comment]]
  case class ListCommentsByPicture(pictureId: Pictures.Id) extends CommentQuery with DSL[List[Comment]]

}
