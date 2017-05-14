package Spit

import Spit.spit._
import akka.actor.FSM.Failure
import akka.actor.Status.Success
import akka.actor.{Actor, ActorPath, ActorRef, ActorSelection, Props}
import akka.pattern.ask

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.concurrent.Future

/*
 Dealer has custody of the piles during gameplay
 dealer receives cards from players and adjudicates disputes
 dealer communicates with players
 */
object Dealer {


  //creates a shuffled deck of 52 playing cards
  def createDeck(): Deck = {
    val suits: List[String] = List("H", "S", "C", "D")
    val deck: Deck = for (
      suit <- suits;
      number <- 1 to 13
    ) yield {
      (number, suit)
    }
    scala.util.Random.shuffle(deck)
  }

  @tailrec
  def deal(cards: Deck, player: ActorRef):Unit = {
    //true to let player know to expect more cards
    if (cards.tail.nonEmpty) player ! SendCard(cards.head)
    else {
      player ! SendCard(cards.head)
      player ! BuildLayout
    }

    if (cards.tail.nonEmpty) deal(cards.tail, player)
  }
}

class Dealer extends Actor() {
  println("Dealer created")

  /*
  Initialisation
   */
  //create players
  val playerOne: ActorRef = context.actorOf(Props[Player], name = "PlayerOne")
  val playerTwo: ActorRef = context.actorOf(Props[Player], name = "PlayerTwo")

  //create list of players and deck
  val players = List(playerOne, playerTwo)
  val initialDeck: Deck = Dealer.createDeck()

  /* gameplay variables */

  var pileOne: Deck = List()
  var pileTwo: Deck = List()

  var playersStuck: Int = 0 //used to ensure that the game doesn't deadlock
  var noPlayersResponded: Int = -1 //used to ensure that both players submit card after deadlock
  var stringCache: List[String] = List() //used to cache layout responses for pretty printing

  /* ******************
    DEALER FUNCTIONS
  ********************* */

  //String repr of dealer layout
  def DealerLayout(): String = "Current table: " + cardToString(pileOne.head) + " " + cardToString(pileTwo.head)

  /*
  Requests a card from each player to be added to the layout
  Start game and breaks deadlocks
   */
  def requestLayoutCards(): Unit = {
    //activates var: used to check if everyone has responded
    noPlayersResponded = 0
    for (p <- players) p ! RequestCard
  }

  //Prints each element of cache and clears cache
  def printCache(): Unit =
    for (i <- stringCache) println(i)
    stringCache = List()

    /*
  Prints current dealer and player layouts.
  Sends current layout to Players to see if they can play card.
  Players need to handle whether they are still stuck or not.
   */
  def resumeGame(): Unit = {

    stringCache = DealerLayout() :: stringCache
    for(i <- players) {
      i ! CurrentLayoutRequest
      Thread.sleep(200)
      i ! Table(List(pileOne.head, pileTwo.head))
    }
    playersStuck = 0

  }

  /*
      MESSAGING

   */

  def receive = {

    /*
    Sends cards to children, children send card back for initial pile
    Used to start game
     */
    case DealCards => {
      println("Dealing...")
      playerOne ! Dealer.deal(initialDeck.take(26), playerOne)
      playerTwo ! Dealer.deal(initialDeck.takeRight(26), playerTwo)
      Thread.sleep(100)
      //get cards from players for the layout
      requestLayoutCards()


    }

    /*
                    **Players send card to dealer**
     In case of deadlock:
    Add card from player to central piles. Doesn't proceed until both players have
    responded. Once players have responded game continues.

    In gameplay:
      Add to pile which is valid for card. Reject if not valid. Dealer must send player a response
      otherwise player will not proceed further.
     */
    case SendCard(card) => {
      //normal gameplay
      if (noPlayersResponded == -1) {
        //check validity
        val valid: List[Int] = cardToValidNumber(card)
        //accept or reject
        if (valid.contains(pileOne.head._1)) {
          println("Dealer accepted " + cardToString(card) + " from " + playerToString(sender()))
          pileOne = card :: pileOne
          sender ! AcceptCard
          resumeGame()
        }
        else if (valid.contains(pileTwo.head._1)) {
          println("Dealer accepted " + cardToString(card) + " from " + playerToString(sender()))
          pileTwo = card :: pileTwo
          sender ! AcceptCard
          resumeGame()
        }
        else sender ! RejectCard(card)

      }
      //dealer requested card to break deadlock
      else {
        if (noPlayersResponded == 0) {
          noPlayersResponded = 1
          pileOne = card :: pileOne
        }
        else if (noPlayersResponded == 1) {
          noPlayersResponded = -1
          pileTwo = card :: pileTwo
          resumeGame()
        }
      }
    }

    case Endgame => {}

    case CurrentLayoutResponse(response) => {
      stringCache = response :: stringCache
      if (stringCache.size == 3) {
        printCache()
      }
    }

    case PlayerStuck => {
      println(playerToString(sender()) + " stuck.")
      playersStuck += 1
      if (playersStuck == 2){
        requestLayoutCards()
        playersStuck = 0
      }
    }

    case DeclaresVictory => {
      println(playerToString(sender()) + " declares Victory!!")
    }


  }
}


/*

    /*
      **********   GAME PLAY  *********
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

    /*
  /*
  Receive card from layout. If card is valid it is added to the stack and a
  receipt is sent to the player. If card is rejected it is returned to the player.
  After each card transaction the dealer's layout is printed and current status
  of the layout is sent to the players.
   */
  def cardReceived(card: Card, sender: ActorRef): Unit = {


    printDealerLayout()
    continue()

  }



*/
  //end of dealer
*/

