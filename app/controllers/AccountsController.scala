package controllers

import akka.actor.ActorSystem
import play.api.libs.json.Json
import play.api.mvc.Controller
import services.account.Accounts

import scala.concurrent.ExecutionContext

/**
  * Created by adelegue on 30/10/2016.
  */
class AccountsController(accounts: Accounts, actorSystem: ActorSystem)(implicit exec: ExecutionContext) extends Controller {


  def getById(id: String) = AuthAction.async {
    import Accounts._
    accounts.getAccountByUsername(id).map {
      case Some(a) => Ok(Json.toJson(a))
      case None => NotFound
    }
  }


}
