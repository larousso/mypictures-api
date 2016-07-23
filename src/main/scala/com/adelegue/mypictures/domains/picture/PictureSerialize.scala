package com.adelegue.mypictures.domains.picture

import java.nio.charset.Charset

import akka.serialization.SerializerWithStringManifest
import org.slf4j.LoggerFactory


/**
  * Created by adelegue on 25/05/2016.
  */
class PictureSerialize extends SerializerWithStringManifest {

  import Pictures._

  val logger = LoggerFactory.getLogger(classOf[PictureSerialize])

  val Utf8 = Charset.forName("UTF-8")

  val PictureCreatedManifest = classOf[PictureCreated].getName
  val PictureUpdatedManifest = classOf[PictureUpdated].getName
  val PictureDeletedManifest = classOf[PictureDeleted].getName

  def identifier = 3

  override def manifest(o: AnyRef): String = o.getClass.getName

  override def fromBinary(bytes: Array[Byte], manifest: String): AnyRef = manifest match {
    case PictureCreatedManifest =>
      pictureCreated(PictureModels.PictureCreated.parseFrom(bytes))
    case PictureUpdatedManifest =>
      pictureUpdated(PictureModels.PictureUpdated.parseFrom(bytes))
    case PictureDeletedManifest =>
      pictureDeleted(PictureModels.PictureDeleted.parseFrom(bytes))
    case _ =>
      throw new IllegalArgumentException("Unable to handle manifest: " + manifest)
  }

  override def toBinary(o: AnyRef): Array[Byte] =
    try {
      o match {
        case PictureCreated(p) =>
          PictureModels.PictureCreated.newBuilder
              .setPicture(getPicture(p)).build().toByteArray
        case PictureUpdated(p) =>
          PictureModels.PictureUpdated.newBuilder
            .setPicture(getPicture(p)).build().toByteArray
        case PictureDeleted(id) =>
          PictureModels.PictureDeleted.newBuilder
            .setId(id).build().toByteArray
      }
  } catch {
    case e: Exception =>
      logger.error(s"Error serializing $o", e)
      e.printStackTrace
      throw e
  }

  def getPicture(a: Picture): PictureModels.Picture = {
    val builder = PictureModels.Picture.newBuilder
      .setId(a.id)
      .setFilename(a.filename)
      .setPreview(a.preview)
      .setType(a.`type`)
      .setAlbum(a.album)
    a.title.foreach(builder.setTitle)
    a.description.foreach(builder.setDescription)
    builder.build()
  }

  private def pictureCreated(a: PictureModels.PictureCreated) = PictureCreated(picture(a.getPicture))

  private def pictureUpdated(a: PictureModels.PictureUpdated) = PictureUpdated(picture(a.getPicture))

  private def pictureDeleted(a: PictureModels.PictureDeleted) = PictureDeleted(a.getId)

  private def picture(p: PictureModels.Picture) = Picture(p.getId, p.getFilename, p.getType, p.getAlbum, p.getPreview, Option(p.getTitle), Option(p.getDescription))

}
