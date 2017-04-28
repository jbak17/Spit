package Spit

import java.util.NoSuchElementException

import Spit.spit._
import akka.actor.Actor

import scala.collection.mutable
import scala.collection.mutable.{ListBuffer, MutableList}

/**
  * Created by jeva on 26/04/17.
  */
class LayoutPile extends Actor(){
  //println("Pile created")

  var pile: mutable.ListBuffer[Card] = ListBuffer()


  def currentPile(): String = cardToString(pile.head) + ("."*(pile.size-1) + " ")


  def receive = {
    case PrintSignal => println(self)
    case CurrentPileRequest => {
      //println(currentPile())
      sender() ! CurrentPileResponse(currentPile())
    }
    case SendSingleCard(card) => {
      pile = pile :+ card
      //println(currentPile())
    }


  }



}
