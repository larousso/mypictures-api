package com.adelegue.mypictures.domains.account

import java.nio.charset.Charset

import akka.serialization.SerializerWithStringManifest

/**
  * Created by adelegue on 25/05/2016.
  */
class AccountSerialize extends SerializerWithStringManifest {
  import Accounts._

  val Utf8 = Charset.forName("UTF-8")

  val AccountAddedManifest = classOf[AccountAdded].getName

  def identifier = 1

  override def manifest(o: AnyRef): String = o.getClass.getName

  override def fromBinary(bytes: Array[Byte], manifest: String): AnyRef = manifest match {
    case AccountAddedManifest =>
      accountAdded(AccountModels.AccountAdded.parseFrom(bytes))
    case _ =>
      throw new IllegalArgumentException("Unable to handle manifest: " + manifest)
  }

  override def toBinary(o: AnyRef): Array[Byte] = o match {
    case AccountAdded(a) =>
      AccountModels.AccountAdded.newBuilder
        .setAccount(AccountModels.Account.newBuilder
          .setName(a.name)
          .setPassword(a.password)
          .setRole(a.role.role)
          .setSurname(a.surname)
          .setUsername(a.username)
          .build()
        ).build().toByteArray
  }

  private def accountAdded(a: AccountModels.AccountAdded) = AccountAdded(account(a.getAccount))

  private def account(p: AccountModels.Account) = Account(p.getUsername, p.getPassword, p.getName, p.getSurname, role(p))

  private def role(p: AccountModels.Account) = if (p.hasRole) Role.fromString(p.getRole) else Role.Guest

}
