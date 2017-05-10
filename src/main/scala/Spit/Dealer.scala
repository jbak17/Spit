package Spit

import Spit.Dealer.PlayerStuck
import Spit.LayoutPile.{AcceptCard, RejectCard}
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

  //create list of players and deck
  val players = List(playerOne, playerTwo)
  val initialDeck: Deck = Dealer.createDeck()

  //print current dealer layout to console
  def printDealerLayout(): Unit = println("Current table: " + cardToString(pileOne.head) + " " + cardToString(pileTwo.head))

  /*
  Receive card from layout. If card is valid it is added to the stack and a
  receipt is sent to the player. If card is rejected it is returned to the player.
  After each card transaction the dealer's layout is printed and current status
  of the layout is sent to the players.
   */
  def cardReceived(card: Card, sender: ActorRef): Unit = {
    val valid: List[Int] = cardToValidNumber(card)
    if (valid.contains(pileOne.head._1)) {
      pileOne = card +: pileOne
      sender ! AcceptCard
    }
    else if (valid.contains(pileTwo.head._1)) {
      pileTwo = card +: pileTwo
      sender ! AcceptCard
    }
    else sender ! RejectCard

    printDealerLayout()
    continue()

  }

  /*
  Sends current layout to Players and prints dealer layout.
  Players need to handle whether they are still stuck or not.
   */
  def continue(): Unit = {
    for(i <- players) i ! CurrentGameState(List(pileOne.head, pileTwo.head))
    printDealerLayout()
    playersStuck = 0
  }

  def receive = {
    //printing
    case PrintTablePiles => printDealerLayout()

    /*
      **********   SET UP  *********
      */

    /*
    Sends cards to children, children send card back for initial pile
     */
    case DealCards => {
      println("Dealing...")
      playerOne ! DealCards(initialDeck.take(26))
      playerTwo ! DealCards(initialDeck.takeRight(26))
    }

    /*
    Starts the game: should only be used once by the inbox.
     */
    case StartGame => continue()



    /*
      **********   GAME PLAY  *********
      */
    /*
    Add card from player to central piles. Doesn't proceed until both players have
    responded. Once players have responded game continues.

    Used for cases when neither player can proceed (deadlock)
     */
    case CardFromPlayerPile(card) => {
      if (sender() == playerOne) pileOne.+=:(card)
      else pileTwo.+=:(card)
      noPlayersResponded += 1
      if (noPlayersResponded == 2){
        noPlayersResponded = 0
        continue()
      }
    }

    /*
    Request from each player the current layout
    Used for preparing console output.
     */
    case CurrentLayoutRequest => for(i <- players) i ! CurrentLayoutRequest

    /*
    Used to receive a single card from a player during gameplay.
    The cardReceived function will determine how to respond to card with
    either accept/reject.
     */
    case SendSingleCard(card) => {
      val sndr: ActorRef = sender()
      cardReceived(card, sndr)
    }

    /*
    Player reports to dealer that they are stuck.
    If both players are stuck the dealer requests a new card from each.
     */
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
