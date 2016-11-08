package services.account

import akka.actor.{ActorSystem, Props}
import akka.persistence.{PersistentActor, SnapshotOffer}
import akka.util.Timeout
import play.api.libs.json._
import services.Messages.{Cmd, Evt, Query}
import services.Persist
import services.account.Accounts._
import services.validation.Validation.Result

import scala.concurrent.Future

/**
  * Created by adelegue on 24/05/2016.
  */
object Accounts {

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

    implicit val format = Format(
      Reads[Role] {
        case JsString(r) => JsSuccess(Role.fromString(r))
        case _ => JsError("Invalid role")
      },
      Writes[Role] { role =>
        JsString(role.role)
      }
    )
  }

  case class Account (username: Username, password: Password, name: Name, surname: Surname, role: Role)

  implicit val format = Json.format[Account]

  sealed trait AccountEvent extends Evt
  case class AccountAdded(account: Account) extends AccountEvent
  case object UnknownAccountEvent extends Evt

  sealed trait AccountCommand extends Cmd
  case class AddAccount(account: Account) extends AccountCommand

  sealed trait AccountQuery extends Query
  case object ListAll extends AccountQuery
  case class GetAccountByUsername(username: Username) extends AccountQuery
}

class Accounts()(implicit val system: ActorSystem) {

  import Accounts._
  import akka.pattern.ask
  import system.dispatcher
  import cats.data.Validated._

  import scala.concurrent.duration.DurationDouble
  implicit val timeout = Timeout(5.second)
  val ref = system.actorOf(AccountStoreActor.props, "Accounts")

  def addAccount(account: Account): Future[Result[Accounts.AccountAdded]] =
    (ref ? AddAccount(account)).mapTo[Result[AccountAdded]]

  def createOrUpdateAccount(account: Account): Future[Result[Accounts.AccountAdded]] = {
    for {
      a <- getAccountByUsername(account.username)
      r <- a match {
        case Some(_) => Future.successful[Result[Accounts.AccountAdded]](valid(AccountAdded(account)))
        case None => addAccount(account)
      }
    } yield r
  }

  def getAccountByUsername(username: Username): Future[Option[Account]] =
    (ref ? GetAccountByUsername(username)).mapTo[Option[Account]]

  def listAll: Future[List[Account]] = (ref ? ListAll).mapTo[List[Account]]

}


object AccountStoreActor {
  def props = Props(classOf[AccountStoreActor])
}

class AccountStoreActor extends PersistentActor {

  import cats.data.Validated._
  override def persistenceId: String = "accounts"

  var state = AccountState()

  def numEvents = state.size

  def updateState[E <: AccountEvent](event: E) = {
    state = state.updated(event)
  }


  val receiveRecover: Receive = {
    case evt: AccountEvent => updateState(evt)
    case SnapshotOffer(_, snapshot: AccountState) => state = snapshot
  }

  val receiveCommand: Receive = {
    case AddAccount(account) =>
      self forward  Persist(AccountAdded(account))
    case Persist(accountEvent: AccountEvent) =>
      persist(accountEvent) { event =>
        updateState(event)
        context.system.eventStream.publish(event)
        sender() ! valid(event)
      }
    case ListAll =>
      sender ! state.accounts.values.toList
    case GetAccountByUsername(username) =>
      sender ! state.accounts.get(username)
    case "snap" => saveSnapshot(state)
    case "print" => println(state)
  }
}

case class AccountState(accounts: Map[Username, Account] = Map.empty) {
  def updated(evt: AccountEvent): AccountState = evt match {
    case AccountAdded(account) =>
      AccountState(accounts + (account.username -> account))
    case _ => this
  }

  def size: Int = accounts.size

  override def toString: String = accounts.toString
}