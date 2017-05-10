package Spit

import akka.actor.{Actor, ActorRef, ActorSystem, Props}

import scala.collection.mutable
import scala.collection.mutable.{ListBuffer, MutableList}
import scala.util.matching.Regex
/**
  * Created by jeva on 17/04/17.
  */

object spit extends App{

  type Suit = String
  type Card = (Int, Suit)
  type Deck = List[Card] //initial deck
  type CardPile = ListBuffer[Card]

  //debugging
  case object Children
  case object Parent

  //  MESSAGES

  //  hand set up
  case object CreateChild
  case object DealCards
  case class DealCards(cards: Deck)

  //  console output
  case object CurrentLayoutRequest
  case object CurrentPileRequest
  case object PrintTablePiles

  //  * * * GAME PLAY *  * *

    /*
  Send message to players with current deck
  Players can return card or inform guardian they are stuck
   */
  case object StartGame
  case object NotifyNoStack

  case object RequestCardFromPlayerDeck //dealer ask player to send card to pile
  case class CurrentGameState(current: Deck)
  case class RequestCardFromLayoutPile(validCards: List[Int])
  case class SendSingleCard(card: Card)
  case class CardFromPlayerPile(card: Card) //is sorted by dealer onto pile
  case class SendMultipleCards(cards: CardPile)



  //responses
  case class CardResponse(card: Card)
  //case class CurrentPileResponse(pile: String)
  case class CurrentLayoutResponse(layout: String)

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

  //create list with valid number value of cards that can be added
  // on top of another card.
  def cardToValidNumber(card: Card): List[Int] = card match {
    case (1, _) => List(13, 2)
    case (13, _) => List(12, 1)
    case (_, _) => List(card._1 +1, card._1 -1)
  }

  def playerToString(actorRef: ActorRef): String = {
    if (actorRef.path.toString.contains("One")) "One"
    else "Two"
  }


   //     ACTOR SYSTEM SETUP

  val system = ActorSystem("Spit_System")
  //create dealer
  val dealer = system.actorOf(Props[Dealer],"Dealer")
  dealer ! DealCards
  dealer ! CurrentLayoutRequest
  Thread.sleep(2000)
  dealer ! StartGame


  //Thread.sleep(6000) //giving 6s for all actors to finish work
  //system.terminate()
}
