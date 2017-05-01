package Spit

import Spit.LayoutPile.{MoveBetweenPiles, RejectCard}
import Spit.Player.{NoCardAvailableFor, NoCardToPlay, PileEmpty}
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
  case class MoveBetweenPiles(card: Card, pile: ActorRef)


}

class LayoutPile extends Actor(){
  //println("Pile created")

  override def supervisorStrategy: SupervisorStrategy = OneForOneStrategy(maxNrOfRetries = 5){
    case _: java.util.NoSuchElementException => Resume
  }

  var pile: mutable.ListBuffer[Card] = ListBuffer()
  var cardLimbo: Card = _
  var dead: Boolean = false

  // String representation of pile
  def currentPile(): String = {
    if (pile.nonEmpty) cardToString(pile.head) + ("."*(pile.size-1) + " ")
    else "X "
  }

  /*
  Considers whether either of the cards on the table piles can be added to with the card on the top of this pile.
  If card exists send to dealer.
   */
  def gameStateResponse(cards: List[Int]) = {

    if (pile.nonEmpty){
      if (cards.contains(pile.head._1))
      {
        dealer ! SendSingleCard(pile.head)
        cardLimbo = pile.head
        println("Layout from " + playerToString(context.parent) + " sent " + cardToString(pile.head) + " to dealer.")
        pile = pile.tail
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
      sender() ! CurrentPileResponse(currentPile())
    }

    //card movements
    case SendSingleCard(card) => {
      pile = pile :+ card
      //println(currentPile())
    }

    case MoveBetweenPiles(card, pileActor) => {
      if (dead == false) pileActor ! SendSingleCard(pile.head)
      else sender() ! NoCardAvailableFor(pileActor)
    }
    // game play updates
    case RequestCardFromLayoutPile(cards) => gameStateResponse(cards)
    case RejectCard => {
      println("Dealer rejected " + cardToString(cardLimbo))
      pile = cardLimbo +: pile
    }

  }



}
