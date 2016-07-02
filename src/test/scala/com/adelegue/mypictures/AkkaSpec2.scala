package com.adelegue.mypictures

import java.io.{File, IOException}
import java.nio.file.attribute.BasicFileAttributes
import java.nio.file._
import java.util.UUID

import akka.actor.ActorSystem
import akka.testkit.{ImplicitSender, TestKit}
import com.typesafe.config.{Config, ConfigFactory}
import org.specs2.mutable.After

import scala.concurrent.ExecutionContext.Implicits.global

/**
  * Created by adelegue on 26/05/2016.
  */
abstract class AkkaSpec2 extends TestKit(ActorSystem()) with After with ImplicitSender {
  def after = system.terminate()
}

class PersistenceConfig {
  private val uuid: UUID = UUID.randomUUID()
  val journalPath = Files.createTempDirectory(s"journal-native$uuid").toFile
  val snapshotPath = Files.createTempDirectory(s"snapshots-native$uuid").toFile

  println(s"Journal path $journalPath")
  println(s"Snapshot path $snapshotPath")

  val config: Config = ConfigFactory.parseString(
    s"""
      |akka {
      |  actor {
      |    serializers {
      |      account = "com.adelegue.mypictures.domains.account.AccountSerialize"
      |      album = "com.adelegue.mypictures.domains.album.AlbumSerialize"
      |      picture = "com.adelegue.mypictures.domains.picture.PictureSerialize"
      |    }
      |    serialization-bindings {
      |      "com.adelegue.mypictures.domains.account.Accounts$$AccountAdded" = account
      |      "com.adelegue.mypictures.domains.album.Albums$$AlbumCreated" = album
      |      "com.adelegue.mypictures.domains.album.Albums$$AlbumUpdated" = album
      |      "com.adelegue.mypictures.domains.album.Albums$$AlbumDeleted" = album
      |      "com.adelegue.mypictures.domains.picture.Pictures$$PictureCreated" = picture
      |      "com.adelegue.mypictures.domains.picture.Pictures$$PictureUpdated" = picture
      |      "com.adelegue.mypictures.domains.picture.Pictures$$PictureDeleted" = picture
      |    }
      |  }
      |}
      |akka.persistence.journal.plugin = "akka.persistence.journal.leveldb"
      |akka.persistence.journal.leveldb.native = off
      |akka.persistence.journal.leveldb.dir = "${journalPath.getAbsolutePath}"
      |akka.persistence.snapshot-store.local.dir = "${snapshotPath.getAbsolutePath}"
    """.stripMargin)
}

object PersistenceConfig {
  def apply(): PersistenceConfig = new PersistenceConfig()
}

abstract class AkkaPersistanceSpecs2(config: PersistenceConfig) extends TestKit(ActorSystem("System", config.config)) with After with ImplicitSender {
  def after = {
    system.terminate().onComplete(any => {
      try {
        DeleteDir(config.journalPath.toPath)
      } catch {
        case e: Exception => println(s"Data directory ${config.journalPath} cleanup failed")
      }

      try {
        DeleteDir(config.snapshotPath.toPath)
      } catch {
        case e: Exception => println(s"Home directory ${config.snapshotPath} cleanup failed")
      }
    })
  }
}

trait WithTmpFolder extends After {
  val tmpFolder = new File(s"target/${UUID.randomUUID}")

  override def after = try {
    DeleteDir(tmpFolder.toPath)
  } catch {
    case e: Exception => println(s"Directory $tmpFolder cleanup failed")
  }
}

abstract class TmpFolderSpec2 extends WithTmpFolder {

}

abstract class AkkaPersistanceWithTmpFolder(config: PersistenceConfig) extends AkkaPersistanceSpecs2(config) with WithTmpFolder {
  override def after = {
    try {
      DeleteDir(tmpFolder.toPath)
    } catch {
      case e: Exception => println(s"Directory $tmpFolder cleanup failed")
    }
    system.terminate().onComplete(any => {
      try {
        DeleteDir(config.journalPath.toPath)
      } catch {
        case e: Exception => println(s"Data directory ${config.journalPath} cleanup failed")
      }
      try {
        DeleteDir(config.snapshotPath.toPath)
      } catch {
        case e: Exception => println(s"Home directory ${config.snapshotPath} cleanup failed")
      }
    })
  }
}

object DeleteDir extends (Path => Unit) {

  override def apply(source: Path): Unit = {

    Files.walkFileTree(source, java.util.EnumSet.of(FileVisitOption.FOLLOW_LINKS), Integer.MAX_VALUE,
      new SimpleFileVisitor[Path]() {
        override def visitFile(file: Path, attrs: BasicFileAttributes): FileVisitResult = {
          Files.delete(file)
          FileVisitResult.CONTINUE
        }
        override def postVisitDirectory(dir: Path, e: IOException): FileVisitResult = {
          if (e == null) {
            Files.delete(dir)
            FileVisitResult.CONTINUE
          } else throw e
        }
      }
    )
  }
}