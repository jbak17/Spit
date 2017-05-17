package Spit

import java.lang.IndexOutOfBoundsException
import java.util.NoSuchElementException

import Spit.spit._
import akka.actor.{Actor, ActorLogging}
import akka.event.LoggingReceive

import util.control.Breaks._
import scala.util.Random.shuffle

/*
Players have their deck of remaining cards and
five piles of cards representing the layout.
Players communicate with the dealer and cardpiles
 */
object Player {

  val emptyCard: Card = (0, "_") //used to signal empty stack

  final def buildLayout(deck: Deck): List[CardPile] = {

    var cards: List[Card] = deck

    var pileOne: CardPile = new CardPile(1)
    var pileTwo: CardPile = new CardPile(2)
    var pileThree: CardPile = new CardPile(3)
    var pileFour: CardPile = new CardPile(4)
    var pileFive: CardPile = new CardPile(5)

    var layout: List[CardPile] = List(pileOne, pileTwo, pileThree, pileFour, pileFive)

    while (cards.nonEmpty)
      for (pile <- layout){
        if (!pile.isFull()) {
          pile.sendCard(cards.head)
          cards = cards.tail
        }
      }
    layout
  }

  /*
  Creates a list of cards that are valid for the current dealer layout.
   */
  def playableCards(cards: Deck): List[Int] = cards.map(card => cardToValidNumber(card)).flatten

  /*
  Checks if the layout is balanced.
   A layout is balanced if there are no piles with more than one card
   if there are any empty piles in layout.
 */
  def isLayoutBalanced(layout: List[CardPile]): Boolean = {
    val empty: Boolean = layout.exists(p => p.isEmpty())
    val gt1: Boolean = layout.exists(p => p.size() > 1)
    if (empty & gt1) false else true
  }

  /*
  Counts empty piles: used when player has less than 5 cards left
  We don't want balance to enter an infinite loop of shuffling cards
  from pile to pile
   */
  def countEmpty(layout: Layout): Int = layout.count(p => p.isEmpty())

  def currentLayoutSize(layout: Layout): Int = layout.map(x => x.size()).foldLeft(0)(_+_)
}


class Player extends Actor  with ActorLogging {
  log.debug("Player created")

  var playerStack: Deck = List()
  var playerLayout: List[CardPile] = List()

  var cardsToWin = 15

  //number of cards accepted by dealer
  var cardsAccepted: Int = 0

  //keep track of most recent table layout
  var currentCards: List[Card] = List.empty

  //flag that player is waiting on dealer to accept card
  var playerLimbo: Boolean = false
  var pileIndex: Int = 0 //in case card is rejected we know which pile to replace

  def buildDeckString(): String = playerToString(self) + " has " + playerStack.size + " in their deck\n"
  def buildLayoutString(layout: Layout): String = {
    var pileStatus: List[(Card, Int, Int)] = List()
    for (pile <- layout) {
      pileStatus = pile.status() :: pileStatus
    }
    pileStatus.sortWith(_._3 < _._3)
    //sorted on third element of tuple
    var outString: List[String] = List()
    for (p <- pileStatus) {
      outString = (cardToString(p._1) + "." * p._2 + " ") :: outString
    }
    playerToString(self) + " layout: " + outString.foldLeft("")(_ + _)

  }


  /*
  At the end of a hand the cards remaining in the layout need to be returned
  to the players stack. Only relevant for losing player.
   */
  def layoutCardsToStack(layout: List[CardPile]): Deck = layout.flatMap(cp => cp.returnCards())

  /*
 Moves cards from full to empty stacks.
  */
  def balanceLayout(currentLayout: List[CardPile]): List[CardPile] = {
    log.debug("Balancing {}", playerToString(self))
    var oldLayout = currentLayout
    //checks if there are empty piles and a pile > 1
    while (!Player.isLayoutBalanced(oldLayout)) {
      try {
        //get empty index
        val emptyIndex: Int = oldLayout.indexOf(oldLayout.filter(CP => CP.isEmpty()).head)
        //get highest index
        val highIndex: Int = oldLayout.map(CP => (oldLayout.indexOf(CP), CP.size())).sortWith(_._2 > _._2).head._1
        //card from highest pile
        val swapCard: Card = oldLayout(highIndex).getCard()
        //add card to empty pile
        oldLayout(emptyIndex).sendCard(swapCard)
      } catch {
        case nse: NoSuchElementException => log.debug("A problem balancing {} with {}", playerToString(self), nse)
          oldLayout
      }

    }
    oldLayout
  }

  def printStartStatus(): Unit = {
    print(buildLayoutString(playerLayout))
    print(buildDeckString())
  }

  def receive = LoggingReceive {

    /*
    Dealer sending card to player.
     */
    case SendCard(card, str) => {
      playerStack = card :: playerStack
      log.debug(playerToString(self) + " received {}", cardToString(card))
    }

    /*
     Builds player layout. If the player has less than 15 cards the dealer is informed
     that the player has entered the endgame.
      */
    case BuildLayout => {
      log.debug(playerToString(self) + "'s stack has " + playerStack.size + " cards.")
      if (playerStack.size > 15) {
        playerLayout = Player.buildLayout(playerStack.take(15))
        playerStack = playerStack.drop(15)

      }
      else {
        playerLayout = Player.buildLayout(playerStack)
        playerStack = List.empty
        dealer ! Endgame
        cardsToWin = playerLayout.size
        log.debug(playerToString(self) + " has entered endgame")
      }
      printStartStatus()

    }

    //dealer accepts
    case AcceptCard => {
      cardsAccepted += 1
      playerLimbo = false
      //check for empty piles and balance
      playerLayout = balanceLayout(playerLayout)
      log.debug(playerToString(self) + "player active")
      println(buildLayoutString(playerLayout))
      if (cardsAccepted == cardsToWin) {
        dealer ! DeclaresVictory
      }
    }

    //dealer rejects
    case RejectCard(card) => {
      synchronized(playerLimbo = false)
      playerLayout(pileIndex).sendCard(card)
      dealer ! Sync
      log.debug(playerToString(self) + "player active")
      log.debug(playerToString(self) + "sent sync request")
    }

    //current cards facing on table
    case Table(deck) => {
      //update cards
      currentCards = deck
      //get list of valid card numbers
      if (!playerLimbo) {
        val validCards: List[Int] = Player.playableCards(deck)

        //contains card
        if (playerLayout.filter(cp => validCards.contains(cp.top()._1)).nonEmpty){
          val pile: CardPile = playerLayout.filter(cp => validCards.contains(cp.top()._1)).head
          pileIndex = playerLayout.indexOf(pile)
          dealer ! SendCard(playerLayout.filter(cp => validCards.contains(cp.top()._1)).head.getCard(), buildLayoutString(playerLayout))
        } else dealer ! PlayerStuck
      }

      /*else if (cardsAccepted + Player.currentLayoutSize(playerLayout) == cardsToWin){
        log.debug(playerToString(self) + " paused: concurrent update error?")
        Thread.sleep(1250)
      }
      */

    }

    //send string repr to dealer
    case CurrentLayoutRequest => {

      dealer ! CurrentLayoutResponse(buildLayoutString(playerLayout))

    }

    //used by dealer to break deadlock/start hand
    case RequestCard => {
      Thread.sleep(500)
      if (playerStack.nonEmpty) {
        sender() ! SendCard(playerStack.head)
        playerStack = playerStack.tail
        log.debug(s"{} sent card on request of dealer.", playerToString(self))
        playerLimbo = false
      }
      else {
        sender() ! SendCard(Player.emptyCard)
        log.debug("{} stack empty.", playerToString(self))
      }
    }

    case Handover => {
      log.debug("Handover commenced")
      playerLimbo = true
      cardsAccepted = 0
      val remainingCards: Deck = layoutCardsToStack(playerLayout)
      log.debug("{} had {} cards remaining at end of hand", playerToString(self), remainingCards.length)
      playerStack = shuffle(playerStack ::: remainingCards)

    }

    case DealerBusy => {}
  }
}
