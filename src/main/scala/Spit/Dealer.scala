package Spit

import Spit.spit._
import akka.actor.{Actor, ActorRef, Props}

import scala.collection.mutable.ListBuffer

/*
 Dealer has custody of the piles during gameplay
 dealer receives cards from players and adjudicates disputes
 dealer communicates with players
 */
class Dealer extends Actor(){
  println("Dealer created")
  var pileOne: CardPile = ListBuffer()
  var pileTwo: CardPile = ListBuffer()

  //create players
  val playerOne: ActorRef = context.actorOf(Props[Player], name = "PlayerOne")
  val playerTwo: ActorRef = context.actorOf(Props[Player], name = "PlayerTwo")

  val players = List(playerOne, playerTwo)
  val initialDeck: Deck = createDeck()
  //sent cards to players

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

  def printDealerLayout(): Unit = println("Current table: " + cardToString(pileOne.head) + " " + cardToString(pileTwo.head))

  def cardReceived(card: Card, sender: ActorRef): Unit = {
    val valid: List[Int] = List(card._1 -1, card._1 +1)
    if (valid.contains(pileOne.head._1)) {
      pileOne = card +: pileOne
    }
    else if (valid.contains(pileTwo.head._1)) {
      pileTwo = card +: pileTwo
    }
    else sender ! RejectCard(card)

    printDealerLayout()
    continue()

  }

  def continue(): Unit = for(i <- players) i ! CurrentGameState(List(pileOne.head, pileTwo.head))

  def receive = {
    //printing
    case Children => {
      println(context.children)
      for (i <- context.children) i ! Children
    }
    case PrintTablePiles => printDealerLayout()

    // setup

    //sends cards to children, children send card back for initial pile
    case DealCards => {
      println("Dealing...")
      playerOne ! DealCards(initialDeck.take(26))
      playerTwo ! DealCards(initialDeck.takeRight(26))
    }
    case StartGame => {
      for(i <- players) i ! CurrentGameState(List(pileOne.head, pileTwo.head))
      printDealerLayout()
    }
    //add card from player to central piles
    case CardFromPlayerPile(card) => {
      if (sender() == playerOne) pileOne += card
      else pileTwo += card
    }

    //  game play
    case CurrentLayoutRequest => for(i <- players) i ! CurrentLayoutRequest
    case SendSingleCard(card) => {
      val sndr: ActorRef = sender()
      cardReceived(card, sndr)
    }

  }
  //end of dealer

}
