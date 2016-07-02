package com.adelegue.mypictures.domains.account

import cats.free.Free
import com.adelegue.mypictures.domains.account.Accounts._
import com.adelegue.mypictures.domains.account.impl.AccountInterpreter
import com.adelegue.mypictures.{AkkaPersistanceSpecs2, PersistenceConfig}
import freek._
import com.adelegue.mypictures.validation.Validation.Result
import org.specs2.mutable.Specification

import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, ExecutionContext, Future}
import scalaz.Success

/**
  * Created by adelegue on 26/05/2016.
  */
class AccountsSpec extends Specification {

  def await[T](f : Future[T]) = Await.result(f, 1 minute)

  "Account store" should {

    "add account" in new AkkaPersistanceSpecs2(PersistenceConfig()) {

      import cats.std.future._
      import ExecutionContext.Implicits.global

      val account = Account("username", "password", "name", "surname", Role.Guest)

      val interpreter = Interpreter(AccountInterpreter(system)).nat

      val res = Accounts.addAccount(account)

      val result: Future[Result[AccountAdded]] = res.foldMap(interpreter)
      val event = await(result)

      event mustEqual Success(AccountAdded(account))

      val account2 = Account("username2", "password", "name", "surname", Role.Guest)
      val event2 = await(Accounts.addAccount(account2).foldMap(interpreter))

      event2 mustEqual Success(AccountAdded(account2))

      await(Accounts.getAccountByUsername("username").foldMap(interpreter)) must beSome(account)
      await(Accounts.getAccountByUsername("username2").foldMap(interpreter)) must beSome(account2)
      await(Accounts.listAll.foldMap(interpreter)) mustEqual List(account, account2)
    }

  }

}
