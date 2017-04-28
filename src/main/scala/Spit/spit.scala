package Spit

import akka.actor.{Actor, ActorRef, ActorSystem, Props}

import scala.collection.mutable
import scala.collection.mutable.MutableList
/**
  * Created by jeva on 17/04/17.
  */

object spit extends App{

  type Suit = String
  type Card = (Int, Suit)
  type Deck = List[Card] //initial deck
  //type CardPile = MutableList[Card]

  //debugging
  case object Children
  case object Parent

  //  MESSAGES
  //  console output
  case object SignalChildren
  case object PrintSignal
  case object CurrentLayoutRequest
  case object CurrentPileRequest
  case object AskName

  //  gameplay
  case object CreateChild
  case object DealCards
  case object NotifyNoStack
  case class AskForCard(current: Deck)
  case class SendSingleCard(card: Card)
  case class SendMultipleCards(cards: Deck)
  case class RejectCard(card: Card)

  //responses
  case class NameResponse(name: String)
  case class CardResponse(card: Card)
  case class CurrentPileResponse(pile: String)
  case class CurrentLayoutResponse(layout: String)

  //creates a shuffled deck of 52 playing cards
  def createDeck(): Deck = {
    val suits: List[Suit] = List("H", "S", "C", "D")
    val deck: Deck = for (
      suit <- suits;
      number <- 1 to 13
    ) yield {
      (number, suit)
    }
    scala.util.Random.shuffle(deck)
  }

  //returns the string representation of a card in form
  // "facevalue suit" without a space, eg. A clubs = AC
  def cardToString(card: Card): String = card match {
    case (10 , _) => "T" + card._2
    case (11, _) => "J" + card._2
    case (12, _) => "Q" + card._2
    case (13, _) => "K" + card._2
    case (1, _) => "A" + card._2
    case (_, _) => card._1.toString + card._2
  }


  /*
   Dealer has custody of the piles during gameplay
   dealer receives cards from players and adjudicates disputes
   dealer communicates with players
   */

  /*
  Players have their deck and layout
  Players communicate with the dealer and layout
   */

  /*
  Layout contains the five piles making the layout
  The layout communicates with the player
  The layout can make changes to the layout without guidance from the player
   */

   //     ACTOR SYSTEM SETUP

  val system = ActorSystem("Spit_System")
  //create dealer
  val dealer = system.actorOf(Props[Dealer],"Dealer")
  dealer ! DealCards
  dealer ! CurrentLayoutRequest


  Thread.sleep(6000) //giving 6s for all actors to finish work
  system.terminate()
}
