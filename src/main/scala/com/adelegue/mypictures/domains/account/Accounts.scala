package com.adelegue.mypictures.domains.account

import akka.actor.{ActorSystem, Props}
import akka.http.scaladsl.server.Route
import akka.persistence.{PersistentActor, SnapshotOffer}
import akka.util.Timeout
import com.adelegue.mypictures.domains.Messages.{Cmd, Evt, Query}
import com.adelegue.mypictures.domains.Persist
import com.adelegue.mypictures.domains.account.Accounts._
import com.adelegue.mypictures.validation.Validation.Result
import org.json4s.JsonAST._
import org.json4s.{CustomSerializer, Formats, Serialization}

import scala.concurrent.Future

/**
  * Created by adelegue on 24/05/2016.
  */
object Accounts {

  case class Api(accounts: Accounts)(implicit system: ActorSystem, serialization: Serialization, formats: Formats) {
    import akka.http.scaladsl.model._
    import akka.http.scaladsl.server.Directives._
    import de.heikoseeberger.akkahttpjson4s.Json4sSupport._

    def route(subRoute: Accounts.Username => Route): Route = {
        pathPrefix("accounts" / "\\w+".r) { username: Accounts.Username =>
          pathEnd {
            get {
              onSuccess(accounts.getAccountByUsername(username)) {
                case Some(a) => complete(a)
                case None => complete(StatusCodes.NotFound)
              }
            }
          } ~
          subRoute(username)
        }
      }
    }

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

  sealed trait AccountEvent extends Evt
  case class AccountAdded(account: Account) extends AccountEvent

  sealed trait AccountCommand extends Cmd
  case class AddAccount(account: Account) extends AccountCommand

  sealed trait AccountQuery extends Query
  case object ListAll extends AccountQuery
  case class GetAccountByUsername(username: Username) extends AccountQuery
}


//trait Accounts {
//  import Accounts._
//
//  def addAccount(account: Account): Future[Result[Accounts.AccountAdded]]
//  def createOrUpdateAccount(account: Account): Future[Result[Accounts.AccountAdded]]
//  def getAccountByUsername(username: Username): Future[Option[Account]]
//  def listAll: Future[List[Account]]
//}

class Accounts()(implicit val system: ActorSystem) {

  import Accounts._
  import system.dispatcher
  import akka.pattern.ask
  import scala.concurrent.duration.DurationDouble
  implicit val timeout = Timeout(5.second)
  val ref = system.actorOf(AccountStoreActor.props, "Accounts")

  def addAccount(account: Account): Future[Result[Accounts.AccountAdded]] =
    (ref ? AddAccount(account)).mapTo[Result[AccountAdded]]

  def createOrUpdateAccount(account: Account): Future[Result[Accounts.AccountAdded]] = {
    import scalaz.Scalaz._
    for {
      a <- getAccountByUsername(account.username)
      r <- a match {
        case Some(_) => Future.successful[Result[Accounts.AccountAdded]](AccountAdded(account).successNel)
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

  import scalaz.Scalaz._
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
        sender() ! event.successNel
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