package com.adelegue.mypictures.domains.account.impl

import akka.actor.{ActorSystem, Props}
import akka.persistence.{PersistentActor, SnapshotOffer}
import akka.util.Timeout
import cats.~>
import com.adelegue.mypictures.domains.Persist
import com.adelegue.mypictures.domains.account.Accounts.{GetAccountByUsername, ListAll, _}
import com.adelegue.mypictures.domains.account._
import com.adelegue.mypictures.validation.Validation.Result

import scala.concurrent.Future
import scala.concurrent.duration.DurationInt
import scalaz.Scalaz._
import scala.language.postfixOps

/**
  * Created by adelegue on 27/05/2016.
  */

object AccountInterpreter {
  def apply(system: ActorSystem): AccountInterpreter = new AccountInterpreter(system)
}

class AccountInterpreter(system: ActorSystem) extends (Accounts.DSL ~> Future) {
  import Accounts._
  import akka.pattern.ask

  val ref = system.actorOf(AccountStoreActor.props)
  implicit val timeout = Timeout(2 second)

  def apply[A](fa: DSL[A]): Future[A] = fa match {
    case c: AddAccount =>
      (ref ? c).mapTo[Result[AccountAdded]].asInstanceOf[Future[A]]
    case ListAll =>
      (ref ? ListAll).mapTo[List[Account]].asInstanceOf[Future[A]]
    case q:GetAccountByUsername =>
      (ref ? q).mapTo[Option[Account]].asInstanceOf[Future[A]]
  }
}



object AccountStoreActor {
  def props = Props(classOf[AccountStoreActor])
}

class AccountStoreActor extends PersistentActor {

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