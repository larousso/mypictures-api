package com.adelegue.mypictures.domains.album

import java.nio.charset.Charset
import java.util.Date
import scala.collection.JavaConversions._
import scala.collection.JavaConverters._

import akka.serialization.SerializerWithStringManifest
import Albums._
import com.adelegue.mypictures.domains.picture.Pictures
/**
  * Created by adelegue on 25/05/2016.
  */
class AlbumSerialize extends SerializerWithStringManifest {

  val Utf8 = Charset.forName("UTF-8")

  val AlbumCreatedManifest = classOf[AlbumCreated].getName
  val AlbumUpdatedManifest = classOf[AlbumUpdated].getName
  val AlbumDeletedManifest = classOf[AlbumDeleted].getName

  def identifier = 2

  override def manifest(o: AnyRef): String = o.getClass.getName

  override def fromBinary(bytes: Array[Byte], manifest: String): AnyRef = manifest match {
    case AlbumCreatedManifest =>
      albumCreated(AlbumModels.AlbumCreated.parseFrom(bytes))
    case AlbumUpdatedManifest =>
      albumUpdated(AlbumModels.AlbumUpdated.parseFrom(bytes))
    case AlbumDeletedManifest =>
      albumDeleted(AlbumModels.AlbumDeleted.parseFrom(bytes))
    case _ =>
      throw new IllegalArgumentException("Unable to handle manifest: " + manifest)
  }

  override def toBinary(o: AnyRef): Array[Byte] = o match {
    case AlbumCreated(a) =>
      AlbumModels.AlbumCreated.newBuilder
        .setAlbum(getAlbumModels(a)).build().toByteArray
    case AlbumUpdated(a) =>
      AlbumModels.AlbumUpdated.newBuilder
        .setAlbum(getAlbumModels(a)).build().toByteArray
    case AlbumDeleted(id) =>
      AlbumModels.AlbumDeleted.newBuilder
        .setId(id)
        .build().toByteArray
  }

  def getAlbumModels(a: Album): AlbumModels.Album = {
    val builder = AlbumModels.Album.newBuilder
      .setId(a.id)
      .setUsername(a.username)
      .setTitle(a.title)
      .setDate(a.date.getTime)
      .addAllPictures(a.pictureIds)
      a.description.foreach(builder.setDescription)
      builder.build()
  }


  private def albumCreated(a: AlbumModels.AlbumCreated) = AlbumCreated(album(a.getAlbum))

  private def albumUpdated(a: AlbumModels.AlbumUpdated) = AlbumUpdated(album(a.getAlbum))

  private def albumDeleted(a: AlbumModels.AlbumDeleted) = AlbumDeleted(a.getId)

  private def album(p: AlbumModels.Album) = Album(p.getId, p.getUsername, p.getTitle, Option(p.getDescription), new Date(p.getDate), getPictures(p))

  private def getPictures(p: AlbumModels.Album): List[Pictures.Id] = Option(p.getPicturesList).map(_.iterator().asScala.toList).getOrElse(List.empty[Pictures.Id])

}
