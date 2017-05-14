package Spit

import Spit.Dealer.{DeclaresVictory, PlayerStuck}
import Spit.LayoutPile.{AcceptCard, RejectCard}
import Spit.Player.DealerAcceptedCard
import Spit.spit.{CurrentLayoutRequest, DealCards, PrintTablePiles, StartGame, _}
import akka.actor.{Actor, ActorPath, ActorRef, ActorSelection, Props}
import akka.pattern.ask

import scala.collection.mutable.ListBuffer
import scala.concurrent.Future

/*
 Dealer has custody of the piles during gameplay
 dealer receives cards from players and adjudicates disputes
 dealer communicates with players
 */
object Dealer {

  //Request player layout: dealer -> player
  case object PrintTablePiles
  //send cards to player: dealer -> player
  case object DealCards
  //dealer signals commencement of game: dealer -> player
  case object StartGame
  //Player signals can't continue: player -> dealer
  case object PlayerStuck
  //Player sends card to dealer: player -> dealer
  case class CardFromPlayerPile(card: Card)
  //Dealer request layout from player: dealer -> player
  case object CurrentLayoutRequest
  //Player informs dealer of victory
  case object DeclaresVictory


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
    val parent: ActorSelection = context.actorSelection(sender.path.parent)
    if (valid.contains(pileOne.head._1)) {
      pileOne = card +: pileOne
      sender ! AcceptCard
    }
    else if (valid.contains(pileTwo.head._1)) {
      pileTwo = card +: pileTwo
      sender ! AcceptCard
      parent ! DealerAcceptedCard
    }
    else sender ! RejectCard

    printDealerLayout()
    continue()

  }

  /*
  Prints current dealer and player layouts.
  Sends current layout to Players to see if they can play card.
  Players need to handle whether they are still stuck or not.
   */
  def continue(): Unit = {
    //val p1Layout: Future[String] = playerOne ? CurrentLayoutRequest
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
    case CardFromPlayerPileToDealerLayout(card) => {
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

    case DeclaresVictory => {
      println("Player " + playerToString(sender()) + " declares victory." )
      context.system.terminate()
    }
  }
  //end of dealer

}
