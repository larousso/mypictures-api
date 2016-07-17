package com.adelegue.mypictures.domains.comment

import java.nio.charset.Charset
import java.util.Date

import akka.serialization.SerializerWithStringManifest
import com.adelegue.mypictures.domains.comment.Comments._

/**
  * Created by adelegue on 25/05/2016.
  */
class CommentSerialize extends SerializerWithStringManifest {

  val Utf8 = Charset.forName("UTF-8")

  val CommentCreatedManifest = classOf[CommentCreated].getName
  val CommentUpdatedManifest = classOf[CommentUpdated].getName
  val CommentDeletedManifest = classOf[CommentDeleted].getName

  def identifier = 4

  override def manifest(o: AnyRef): String = o.getClass.getName

  override def fromBinary(bytes: Array[Byte], manifest: String): AnyRef = manifest match {
    case CommentCreatedManifest =>
      commentCreated(CommentModels.CommentCreated.parseFrom(bytes))
    case CommentUpdatedManifest =>
      commentUpdated(CommentModels.CommentUpdated.parseFrom(bytes))
    case CommentDeletedManifest =>
      commentDeleted(CommentModels.CommentDeleted.parseFrom(bytes))
    case _ =>
      throw new IllegalArgumentException("Unable to handle manifest: " + manifest)
  }

  override def toBinary(o: AnyRef): Array[Byte] = o match {
    case CommentCreated(a) =>
      CommentModels.CommentCreated.newBuilder
        .setComment(getCommentModels(a)).build().toByteArray
    case CommentUpdated(a) =>
      CommentModels.CommentUpdated.newBuilder
        .setComment(getCommentModels(a)).build().toByteArray
    case CommentDeleted(id) =>
      CommentModels.CommentDeleted.newBuilder
        .setId(id)
        .build().toByteArray
  }

  def getCommentModels(a: Comment): CommentModels.Comment = {
    CommentModels.Comment.newBuilder
      .setId(a.id)
      .setPictureId(a.pictureId)
      .setName(a.name)
      .setDate(a.date.getTime)
      .setComment(a.comment)
      .build()
  }

  private def commentCreated(a: CommentModels.CommentCreated) = CommentCreated(comment(a.getComment))

  private def commentUpdated(a: CommentModels.CommentUpdated) = CommentUpdated(comment(a.getComment))

  private def commentDeleted(a: CommentModels.CommentDeleted) = CommentDeleted(a.getId)

  private def comment(p: CommentModels.Comment) = Comment(p.getId, p.getPictureId, p.getName, p.getComment, new Date(p.getDate))

}
