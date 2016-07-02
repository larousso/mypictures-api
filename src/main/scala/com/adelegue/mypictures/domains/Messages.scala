package com.adelegue.mypictures.domains

/**
  * Created by adelegue on 28/05/2016.
  */
object Messages {
  trait Evt
  trait Cmd
  trait Query
}

case class Persist[Event <: Messages.Evt](evt: Event)

class UnknowEventException extends RuntimeException