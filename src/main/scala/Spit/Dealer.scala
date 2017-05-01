package Spit

import Spit.Dealer.PlayerStuck
import Spit.LayoutPile.RejectCard
import Spit.spit.{Children, CurrentLayoutRequest, DealCards, PrintTablePiles, StartGame, _}
import akka.actor.{Actor, ActorRef, Props}

import scala.collection.mutable.ListBuffer

/*
 Dealer has custody of the piles during gameplay
 dealer receives cards from players and adjudicates disputes
 dealer communicates with players
 */
object Dealer {

  case object Children

  case object PrintTablePiles

  case object DealCards

  case object StartGame

  case object PlayerStuck

  case class CardFromPlayerPile(card: Card)

  case object CurrentLayoutRequest


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
}

class Dealer extends Actor(){
  println("Dealer created")
  var pileOne: CardPile = ListBuffer()
  var pileTwo: CardPile = ListBuffer()

  var playersStuck = 0 //used to ensure that the game doesn't deadlock
  var noPlayersResponded = 0 //used to ensure that both players submit card after deadlock

  //create players
  val playerOne: ActorRef = context.actorOf(Props[Player], name = "PlayerOne")
  val playerTwo: ActorRef = context.actorOf(Props[Player], name = "PlayerTwo")

  val players = List(playerOne, playerTwo)
  val initialDeck: Deck = Dealer.createDeck()
  //sent cards to players

  def printDealerLayout(): Unit = println("Current table: " + cardToString(pileOne.head) + " " + cardToString(pileTwo.head))

  def cardReceived(card: Card, sender: ActorRef): Unit = {
    val valid: List[Int] = cardToValidNumber(card)
    if (valid.contains(pileOne.head._1)) {
      pileOne = card +: pileOne
    }
    else if (valid.contains(pileTwo.head._1)) {
      pileTwo = card +: pileTwo
    }
    else sender ! RejectCard

    printDealerLayout()
    continue()

  }

  def continue(): Unit = {
    for(i <- players) i ! CurrentGameState(List(pileOne.head, pileTwo.head))
    printDealerLayout()
  }

  def receive = {
    //printing
    case PrintTablePiles => printDealerLayout()

    // setup

    //sends cards to children, children send card back for initial pile
    case DealCards => {
      println("Dealing...")
      playerOne ! DealCards(initialDeck.take(26))
      playerTwo ! DealCards(initialDeck.takeRight(26))
    }
    case StartGame => continue()

    //add card from player to central piles
    case CardFromPlayerPile(card) => {
      if (sender() == playerOne) pileOne += card
      else pileTwo += card
      noPlayersResponded += 1
      if (noPlayersResponded == 2){
        noPlayersResponded = 0
        continue()
      }
    }

    //  game play
    case CurrentLayoutRequest => for(i <- players) i ! CurrentLayoutRequest
    case SendSingleCard(card) => {
      val sndr: ActorRef = sender()
      cardReceived(card, sndr)
    }
    case PlayerStuck => {
      playersStuck += 1
      if (playersStuck == 2){
        for(i <- players) i ! RequestCardFromPlayerDeck
        playersStuck = 0
      }
      println("Player " + playerToString(sender()) + " stuck" )
      sender() ! CurrentLayoutRequest
    }

  }
  //end of dealer

}
