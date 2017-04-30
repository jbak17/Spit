package Spit

import Spit.spit._
import akka.actor.SupervisorStrategy.Resume
import akka.actor.{Actor, OneForOneStrategy, SupervisorStrategy}

import scala.collection.mutable
import scala.collection.mutable.{ListBuffer, MutableList}

/*
LayoutPiles represent the five piles making the layout
The layout communicates with the player
The layout can notify the player if a pile is empty and request a card
 */


class LayoutPile extends Actor(){
  //println("Pile created")

  override def supervisorStrategy: SupervisorStrategy = OneForOneStrategy(maxNrOfRetries = 5){
    case _: java.util.NoSuchElementException => Resume
  }

  var pile: mutable.ListBuffer[Card] = ListBuffer()

  // String representation of pile
  def currentPile(): String = cardToString(pile.head) + ("."*(pile.size-1) + " ")

  /*
  Considers whether either of the cards on the table piles can be added to with the card on the top of this pile.
  If card exists send to dealer.
   */
  def gameStateResponse(cards: List[Int]) = {

    if (pile.nonEmpty){
      if (cards.contains(pile.head._1))
      {
        dealer ! SendSingleCard(pile.head)
        println("Layout from " + playerToString(context.parent) + " sent " + cardToString(pile.head) + " to dealer.")
        pile = pile.tail
      }
    }
  }


  def receive = {
    //printing
    case PrintSignal => println(self)
    case CurrentPileRequest => {
      //println(currentPile())
      sender() ! CurrentPileResponse(currentPile())
    }

    //card movements
    case SendSingleCard(card) => {
      pile = pile :+ card
      //println(currentPile())
    }

    // game play updates
    case RequestCardFromLayoutPile(cards) => gameStateResponse(cards)

  }



}
