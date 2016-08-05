package com.adelegue.mypictures.domains.album

import java.util.{Date, UUID}

import akka.actor.ActorSystem
import akka.http.scaladsl.marshalling._
import akka.http.scaladsl.server._
import akka.http.scaladsl.unmarshalling.FromEntityUnmarshaller
import cats.free.Free
import com.adelegue.mypictures.domains.Auth
import com.adelegue.mypictures.domains.Messages.{Cmd, Evt, Query}
import com.adelegue.mypictures.domains.account.Accounts
import com.adelegue.mypictures.domains.account.Accounts.Role.Admin
import com.adelegue.mypictures.domains.account.Accounts.Username
import com.adelegue.mypictures.domains.picture.Pictures
import com.adelegue.mypictures.validation.Validation._
import freek._
import org.json4s.{Formats, Serialization}

import scala.concurrent.Future
import scalaz.{Failure, Success}

/**
  * Created by adelegue on 28/05/2016.
  */


object Albums {

  object Api {

    case class Album(title: Albums.Title, description: Option[Albums.Description], date: Date = new Date())

  }


  case class Api(auth: Auth, interpreter: Interpreter[PRG.Cop, Future], pictureInterpreter: Interpreter[Pictures.PRG.Cop, Future])(implicit system: ActorSystem, serialization: Serialization, formats: Formats) {

    import cats.std.future._
    import system.dispatcher
    import akka.http.scaladsl.model._
    import akka.http.scaladsl.server.Directives._
    import de.heikoseeberger.akkahttpjson4s.Json4sSupport._

    def readAlbum(albumId: Albums.Id): Route = get {
      onSuccess(Albums.getAlbum(albumId).interpret(interpreter)) {
        case Some(a) => complete(a)
        case None => complete(StatusCodes.NotFound)
      }
    }

    def route(username: Accounts.Username)(subRoute: Albums.Id => Route): Route =
      auth.isAuthenticated {
        pathPrefix("albums") {
          pathEnd {
            get {
              onSuccess(Albums.getAlbumByUsername(username).interpret(interpreter)) { albums =>
                complete(albums)
              }
            } ~
              post {
                auth.hashRole(Admin) {
                  entity(as[Api.Album]) {
                    case Api.Album(title, description, date) =>
                      onSuccess(Albums.createAlbum(Albums.Album(UUID.randomUUID.toString, username, title, description, date)).interpret(interpreter)) {
                        case Success(a) => complete(StatusCodes.Created -> a.album)
                        case Failure(e) => complete(StatusCodes.BadRequest -> e)
                      }
                  }
                }
              }
          } ~
            pathPrefix("[a-z0-9\\-]+".r) { albumId: Albums.Id =>
              pathEnd {
                readAlbum(albumId) ~
                  delete {
                    auth.hashRole(Admin) {
                      val res = for {
                        _ <- Pictures.deletePicturesByAlbum(albumId)
                        delete <- Albums.deleteAlbum(albumId).expand[Pictures.PRG]
                      } yield delete

                      onSuccess(res.interpret(pictureInterpreter)) {
                        case Success(a) =>
                          complete(StatusCodes.OK -> a)
                        case Failure(e) =>
                          complete(StatusCodes.BadRequest -> e)
                      }
                    }
                  } ~
                  put {
                    auth.hashRole(Admin) {
                      entity(as[Albums.Album]) { album =>
                        onSuccess(Albums.updateAlbum(album).interpret(interpreter)) {
                          case Success(a) =>
                            complete(StatusCodes.OK -> a.album)
                          case Failure(e) =>
                            complete(StatusCodes.BadRequest -> e)
                        }
                      }
                    }
                  }
              } ~
                subRoute(albumId)
            }
        }
      }
    }


  type PRG = Albums.DSL :|: Accounts.PRG
  val PRG = Program[PRG]

  def createAlbum(album: Album): Free[PRG.Cop, Result[AlbumCreated]] =
    for {
      validatedAlbum <- Validation.validateAlbumCreation(album)
      command <- validatedAlbum.fold(
        e => Free.pure[PRG.Cop, Result[AlbumCreated]](Failure(e)),
        s => CreateAlbum(album).freek[PRG]
      )
    } yield command

  def updateAlbum(album: Album): Free[PRG.Cop, Result[AlbumUpdated]] =
    for {
      validatedAlbum <- Validation.validateAlbumUpdate(album)
      command <- validatedAlbum.fold(
        e => Free.pure[PRG.Cop, Result[AlbumUpdated]](Failure(e)),
        s => UpdateAlbum(album).freek[PRG]
      )
    } yield command

  def deleteAlbum(id: Id): Free[PRG.Cop, Result[AlbumDeleted]] =
    for {
      delete <- DeleteAlbum(id).freek[PRG]
    } yield delete

  def getAlbum(id: Id): Free[PRG.Cop, Option[Album]] =
    for {get <- GetAlbum(id).freek[PRG]} yield get

  def getAlbumByUsername(username: Username): Free[PRG.Cop, List[Album]] =
    for {get <- GetAlbumByUsername(username).freek[PRG]} yield get

  def listAll: Free[PRG.Cop, List[Album]] = for {list <- ListAlbums.freek[PRG]} yield list

  object Validation {

    import com.adelegue.mypictures.validation.Validation._

    import scalaz.Scalaz._

    def validateAlbumCreation(album: Album): Free[PRG.Cop, Result[Album]] =
      for {
        username <- validateUsername(album)
        alreadyExists <- validateNotExists(album)
      } yield (username |@| alreadyExists) { (_, _) => album }


    def validateAlbumUpdate(album: Album): Free[PRG.Cop, Result[Album]] =
      for {
        username <- validateUsername(album)
        alreadyExists <- validateExists(album)
      } yield (username |@| alreadyExists) { (_, _) => album }

    def validateUsername(album: Album): Free[PRG.Cop, Result[Album]] =
      Accounts.getAccountByUsername(album.username).expand[PRG].map {
        case Some(a) => album.successNel
        case None => Error(s"L'utilisateur ${album.username} n'existe pas").failureNel
      }

    def validateNotExists(album: Album): Free[PRG.Cop, Result[Album]] =
      albumExists(album).map { exists => if (exists) Error("L'album existe déjà").failureNel else album.successNel }

    def validateExists(album: Album): Free[PRG.Cop, Result[Album]] =
      albumExists(album).map { exists => if (!exists) Error("L'album n'existe pas").failureNel else album.successNel }

    def albumExists(album: Album): Free[PRG.Cop, Boolean] =
      for {
        mayBe <- Albums.getAlbum(album.id)
      } yield mayBe.isDefined
  }


  type Id = String
  type Title = String
  type Description = String

  case class Album(id: Id, username: Username, title: Title, description: Option[Description], date: Date = new Date())

  sealed trait DSL[A]

  sealed trait AlbumCommand extends Cmd

  case class CreateAlbum(album: Album) extends AlbumCommand with DSL[Result[AlbumCreated]]

  case class UpdateAlbum(album: Album) extends AlbumCommand with DSL[Result[AlbumUpdated]]

  case class DeleteAlbum(id: Id) extends AlbumCommand with DSL[Result[AlbumDeleted]]

  sealed trait AlbumEvent extends Evt

  case class AlbumCreated(album: Album) extends AlbumEvent

  case class AlbumUpdated(album: Album) extends AlbumEvent

  case class AlbumDeleted(id: Id) extends AlbumEvent

  sealed trait AlbumQuery extends Query

  case class GetAlbum(id: Id) extends AlbumQuery with DSL[Option[Album]]

  case class GetAlbumByUsername(username: Username) extends AlbumQuery with DSL[List[Album]]

  case object ListAlbums extends AlbumQuery with DSL[List[Album]]

}