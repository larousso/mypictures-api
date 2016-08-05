package com.adelegue.mypictures.domains.account

import akka.actor.ActorSystem
import akka.http.scaladsl.marshalling._
import akka.http.scaladsl.model.HttpResponse
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.unmarshalling.FromEntityUnmarshaller
import com.adelegue.mypictures.domains.Messages.{Cmd, Evt, Query}
import com.adelegue.mypictures.validation.Validation.Result
import cats.free.Free
import freek._
import org.json4s.{CustomSerializer, Formats, Serialization}
import org.json4s.JsonAST._

import scala.concurrent.Future
/**
  * Created by adelegue on 24/05/2016.
  */
object Accounts {

  case class Api(interpreter: Interpreter[PRG.Cop, Future])(implicit system: ActorSystem, serialization: Serialization, formats: Formats) {
    import cats.std.future._
    import system.dispatcher
    import akka.http.scaladsl.model._
    import akka.http.scaladsl.server.Directives._
    import de.heikoseeberger.akkahttpjson4s.Json4sSupport._

    def route(subRoute: Accounts.Username => Route): Route = {
        pathPrefix("accounts" / "\\w+".r) { username: Accounts.Username =>
          pathEnd {
            get {
              onSuccess(Accounts.getAccountByUsername(username).interpret(interpreter)) {
                case Some(a) => complete(a)
                case None => complete(StatusCodes.NotFound)
              }
            }
          } ~
          subRoute(username)
        }
      }
    }

  type PRG = DSL :|: FXNil
  val PRG = Program[PRG]

  def addAccount(account: Account): Free[PRG.Cop, Result[Accounts.AccountAdded]] =
    for {
      add <- AddAccount(account).freek[PRG]
    } yield add

  def createOrUpdateAccount(account: Account): Free[PRG.Cop, Result[Accounts.AccountAdded]] = {
    import scalaz.Scalaz._
    for {
      a <- getAccountByUsername(account.username)
      r <- a match {
        case Some(a) => Free.pure[PRG.Cop, Result[Accounts.AccountAdded]](AccountAdded(account).successNel)
        case None => for {
          added <- addAccount(account)
        } yield added
      }
    } yield r
  }

  def getAccountByUsername(username: Username): Free[PRG.Cop, Option[Account]] =
    for {
      get <- GetAccountByUsername(username).freek[PRG]
    } yield get

  def listAll: Free[PRG.Cop, List[Account]] =
    for {
      list <- ListAll.freek[PRG]
    } yield list

  case class Account (username: Username, password: Password, name: Name, surname: Surname, role: Role)

  type Username = String
  type Password = String
  type Name = String
  type Surname = String


  sealed abstract class Role { def role: String}
  object Role {
    case object Admin extends Role { override val role = "Admin" }
    case object Guest extends Role { override val role = "Guest" }
    case object SuperGuest extends Role { override val role = "Superguest" }
    def fromString(s: String) = s match {
      case Admin.role => Admin
      case Guest.role  => Guest
      case SuperGuest.role  => SuperGuest
      case _           => Guest
    }
  }

  class RoleSerializer extends CustomSerializer[Role](format => (
    {
      case JString(s) if s == Role.Admin.role =>
        Role.Admin
      case JString(s) if s == Role.Guest.role =>
        Role.Guest
      case JString(s) if s == Role.SuperGuest.role =>
        Role.SuperGuest
    }, {
    case n: Role =>
      JString(n.role)
  }))

  sealed trait DSL[A]

  sealed trait AccountEvent extends Evt
  case class AccountAdded(account: Account) extends AccountEvent

  sealed trait AccountCommand extends Cmd
  case class AddAccount(account: Account) extends DSL[Result[AccountAdded]] with AccountCommand

  sealed trait AccountQuery extends Query
  case object ListAll extends AccountQuery with DSL[List[Account]]
  case class GetAccountByUsername(username: Username) extends AccountQuery with DSL[Option[Account]]
}
