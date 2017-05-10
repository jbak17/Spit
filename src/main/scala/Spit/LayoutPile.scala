package Spit

import Spit.LayoutPile.{AcceptCard, MoveBetweenPiles, RejectCard, SendCardToPile}
import Spit.Player.{CurrentPileResponse, NoCardAvailableFor, NoCardToPlay, PileEmpty}
import Spit.spit._
import akka.actor.SupervisorStrategy.Resume
import akka.actor.{Actor, ActorRef, OneForOneStrategy, SupervisorStrategy}

import scala.collection.mutable
import scala.collection.mutable.{ListBuffer, MutableList}

/*
LayoutPiles represent the five piles making the layout
The layout communicates with the player
The layout can notify the player if a pile is empty and request a card
 */
object LayoutPile {

  case object RejectCard
  case object AcceptCard

  case class MoveBetweenPiles(card: Card)
  case class SendCardToPile(sndr: ActorRef)


}

class LayoutPile extends Actor(){
  //println("Pile created")

  override def supervisorStrategy: SupervisorStrategy = OneForOneStrategy(maxNrOfRetries = 5){
    case _: java.util.NoSuchElementException => Resume
  }

  var pile: mutable.ListBuffer[Card] = ListBuffer()
  var cardLimbo: Boolean = false
  var dead: Boolean = false

  // String representation of pile
  def currentPile(): String = {
    if (pile.nonEmpty) cardToString(pile.head) + ("."*(pile.size-1) + " ")
    else "X "
  }

  /*
  Considers whether either of the cards on the table piles can be added to with the card on the top of this pile.
  If card exists send to dealer.
  Card goes into limbo until dealer confirms acceptance.
  If dealer rejects, card is added back to pile.
   */
  def gameStateResponse(cards: List[Int]) = {

    if (pile.nonEmpty & cardLimbo != true){
      if (cards.contains(pile.head._1))
      {
        dealer ! SendSingleCard(pile.head)
        cardLimbo = true
        println("Layout from " + playerToString(context.parent) + " sent " + cardToString(pile.head) + " to dealer.")
      }
      else context.parent ! NoCardToPlay
    }
    else if (dead == false) {
      dead = true
      context.parent ! PileEmpty
    }
  }


  def receive = {
    //printing
    case CurrentPileRequest => {
      //println(currentPile())
      sender() ! CurrentPileResponse(currentPile(), pile.length)
    }

    //card movements
    case SendSingleCard(card) => {
      pile = pile :+ card
      //println(currentPile())
    }

    /*
    Used to respond to request from player to provide a card to another pile
     */
    case SendCardToPile(sndr) => {
      if (pile.length != 0) {
        sndr ! MoveBetweenPiles(pile.head)
        pile.remove(0)
      }
      else sender() ! NoCardAvailableFor(sndr)
    }

    /*
    Used for when a pile receives a card from another pile
     */
    case MoveBetweenPiles(card) => {
      pile = pile :+ card
    }

    // game play updates
    case RequestCardFromLayoutPile(cards) => gameStateResponse(cards)

    case RejectCard => {
      println("Dealer rejected " + cardToString(pile.head))
      cardLimbo = false

    }
    case AcceptCard =>{
      if (pile.nonEmpty) pile = pile.tail
      else context.parent ! PileEmpty
    }

  }



}
