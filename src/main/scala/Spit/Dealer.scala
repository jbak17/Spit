package Spit

import java.util.NoSuchElementException

import Spit.spit._
import akka.actor.{Actor, ActorLogging, ActorPath, ActorRef, ActorSelection, Props}

import scala.annotation.tailrec

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

  /*
  Sends a succession of cards to a player.
   */
  @tailrec
  def deal(cards: Deck, player: ActorRef):Unit = {
    if (cards.tail.nonEmpty) player ! SendCard(cards.head)
    else {
      player ! SendCard(cards.head)
      player ! BuildLayout
    }

    if (cards.tail.nonEmpty) deal(cards.tail, player)
  }
}

class Dealer extends Actor with ActorLogging {
  log.debug("Dealer created")

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

  var victoryDeclared: Boolean = false //used to ensure dealer doesn't reset game twice.

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
    log.debug("Dealer requests layout cards")
    noPlayersResponded = 0
    for (p <- players) p ! RequestCard
  }

  //Prints each element of cache and clears cache
  def printCache(): Unit = {
    log.debug("Dealer printing cache")
    for (i <- stringCache) println(i)
    stringCache = List()
  }

    /*
  Prints current dealer and player layouts.
  Sends current layout to Players to see if they can play card.
  Players need to handle whether they are still stuck or not.
   */
  def resumeGame(): Unit = {

    stringCache = DealerLayout() :: stringCache
    for(i <- players) {
      i ! CurrentLayoutRequest //get strings of layouts from players
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
      log.debug("Dealer dealing...")

    }

    case Sync => {
      if (pileOne.nonEmpty & pileTwo.nonEmpty) {
        sender() ! Table(List(pileOne.head, pileTwo.head))
        log.debug("Dealer sent Table to {}", playerToString(sender()))
      }
      else {
        Thread.sleep(100)
        if (pileOne.nonEmpty & pileTwo.nonEmpty) sender() ! Table(List(pileOne.head, pileTwo.head))
        else {
          sender() ! DealerBusy
          log.debug("Dealer busy")
        }
      }
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
          log.debug("Dealer accepted card {}", cardToString(card))
          println("Dealer accepted " + cardToString(card) + " from " + playerToString(sender()))
          pileOne = card :: pileOne
          sender ! AcceptCard
          resumeGame()
        }
        else if (valid.contains(pileTwo.head._1)) {
          log.debug("Dealer accepted card {}", cardToString(card))
          println("Dealer accepted " + cardToString(card) + " from " + playerToString(sender()))
          pileTwo = card :: pileTwo
          sender ! AcceptCard
          resumeGame()
        }
        else {
          sender ! RejectCard(card)
          log.debug("Dealer rejected card {}", cardToString(card))
        }

      }
      //dealer requested card to break deadlock
      else {
        if (noPlayersResponded == 0) {
          log.debug("1. {} sent {} on request of dealer", playerToString(sender()), cardToString(card))
          noPlayersResponded = 1
          //empty card
          if (card._1 != 0) pileOne = card :: pileOne
        }
        else if (noPlayersResponded == 1) {
          log.debug("2. {} sent {} on request of dealer", playerToString(sender()), cardToString(card))
          noPlayersResponded = -1 //reset to normal gamestate
          if (card._1 != 0) pileTwo = card :: pileTwo
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
      synchronized {
        log.debug("{} stuck", playerToString(sender()))
        println(playerToString(sender()) + " stuck.")
        playersStuck += 1
        if (playersStuck == 2){
          requestLayoutCards()
          playersStuck = 0
        }
      }
    }
    /*
    Occurs when player declares victory.
    The loser will need to move their cards to their stack.
    The winner gets the lesser pile, on the assumption that we need
    some way of rewarding victory.
     */
    case DeclaresVictory => {
      println(playerToString(sender()) + " declares Victory!!")
      log.debug(playerToString(sender()) + " declares Victory!!")

      for (p <- players) p ! Handover //tells players to stop what they're doing

      val shortpile: List[Card] = if (pileOne.length < pileTwo.length) pileOne else pileTwo
      val longpile: List[Card] = if (pileOne.length > pileTwo.length) pileOne else pileTwo


      val winner: ActorRef = sender()
      val loser: ActorRef = if (sender() == playerOne) playerTwo else playerOne

      //send cards to players
      //reset dealer piles.
      pileOne = shortpile.head :: List()
      pileTwo = longpile.head :: List()
      Thread.sleep(200)
      try {
        Dealer.deal(shortpile.tail, winner) //short stack to winner
        Dealer.deal(longpile.tail, loser) //long stack to loser
        log.debug("Players sent new cards")
      } catch {
        case el: NoSuchElementException => {
          Thread.sleep(200)
          Dealer.deal(shortpile.tail, winner) //short stack to winner
          Dealer.deal(longpile.tail, loser) //long stack to loser
        }
      }


      //Resume game
      Thread.sleep(200)
      resumeGame()
    }


  }
}


