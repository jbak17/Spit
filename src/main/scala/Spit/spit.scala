package Spit

import akka.actor.{Actor, ActorRef, ActorSystem, Props}

import scala.collection.mutable
import scala.collection.mutable.{ListBuffer, MutableList}
import scala.util.matching.Regex
/**
  * Created by jeva on 17/04/17.
  */

object spit extends App{

  type Card = (Int, String)
  type Deck = List[Card] //initial deck

  //  MESSAGES

  /*
    All actors to recieve
  */
  case class SendCard(card: Card)

  /*
  Player to deal with
   */
  case object AcceptCard //dealer accepts
  case class RejectCard(card: Card) //dealer rejects
  case class Table(deck: Deck) //current cards facing on table
  case object CurrentLayoutRequest //send string repr to dealer
  case object RequestCard //used by dealer to break deadlock/start hand
  case object BuildLayout //used to start hand.

  /*
  Dealer to manage
   */
  case object Endgame //player declares non-full layout
  case class CurrentLayoutResponse(layout: String) //player shows layout to dealer
  case object PlayerStuck //player with no card to play.
  case object DealCards //used to start game.
  case object DeclaresVictory
  /*
  returns the string representation of a card in form
  "facevalue suit" without a space, eg. A clubs = AC
   */
  def cardToString(card: Card): String = card match {
    case (10 , _) => "T" + card._2
    case (11, _) => "J" + card._2
    case (12, _) => "Q" + card._2
    case (13, _) => "K" + card._2
    case (1, _) => "A" + card._2
    case (0, _) => "X" //empty card for layout display.
    case (_, _) => card._1.toString + card._2
  }

  /*
  returns string representation of player in form: "Player X"
   */
  def playerToString(actorRef: ActorRef): String = {
    if (actorRef.path.toString.contains("One")) "Player One"
    else "Player Two"
  }

  /*
  create list with valid number value of cards that can be added
  on top of another card.
  Used to translate dealer layout cards into matches for players.
   */
  def cardToValidNumber(card: Card): List[Int] = card match {
    case (1, _) => List(13, 2)
    case (13, _) => List(12, 1)
    case (_, _) => List(card._1 +1, card._1 -1)
  }

  def initialiseGame(): Unit = {
    dealer ! DealCards
    Thread.sleep(500)
  }

   //     ACTOR SYSTEM SETUP

  val system = ActorSystem("Spit_System")
  //create dealer
  val dealer = system.actorOf(Props[Dealer],"Dealer")
  initialiseGame()


  //Thread.sleep(6000) //giving 6s for all actors to finish work
  //system.terminate()
}
