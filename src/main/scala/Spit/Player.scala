package Spit

import Spit.spit._
import akka.actor.{Actor, ActorRef, Props}

import util.control.Breaks._
import scala.concurrent.{Await, Future}
import akka.pattern.ask

/*
Players have their deck of remaining cards and
five piles of cards representing the layout.
Players communicate with the dealer and cardpiles
 */
object Player {



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

}


class Player extends Actor() {
  println("Player created")

  var playerStack: Deck = List()
  var playerLayout: List[CardPile] = List()

  val cardsToWin = 15

  //number of cards accepted by dealer
  var cardsAccepted: Int = 0

  //keep track of piles that can play, used to prevent deadlock.
  var pilesWithNoCardToPlay: Int = 0

  //flag that player is waiting on dealer to accept card
  var playerLimbo: Boolean = false
  var pileIndex: Int = 0 //in case card is rejected we know which pile to replace

  def buildLayoutString(): String = {
    var pileStatus: List[(Card, Int, Int)] = List()
    for (pile <- playerLayout) {
      pileStatus = pile.status() :: pileStatus
    }
    pileStatus.sortWith(_._3 < _._3)
    //sorted on third element of tuple
    var outString: List[String] = List()
    for (p <- pileStatus) {
      outString = (cardToString(p._1) + "." * p._2 + " ") :: outString
    }
    playerToString(self) + "'s layout: " + outString.foldLeft("")(_ + _)
  }


  def receive = {

    /*
    Dealer sending card to player.
     */
    case SendCard(card) => playerStack = card :: playerStack

    /*
    Player to deal with
     */
    /*
     Builds player layout. If the player has less than 15 cards the dealer is informed
     that the player has entered the endgame.
      */
    case BuildLayout => {
      println(s"Player's stack has " + playerStack.size + " cards.")
      if (playerStack.size > 15) {
        playerLayout = Player.buildLayout(playerStack.take(15))
        playerStack = playerStack.drop(15)
      }
      else {
        playerLayout = Player.buildLayout(playerStack)
        playerStack = List()
        dealer ! Endgame
      }

    }

    //dealer accepts
    case AcceptCard => {
      cardsAccepted += 1
      playerLimbo = false
      println(buildLayoutString())
      if (cardsAccepted == cardsToWin) {
        dealer ! DeclaresVictory
      }
    }

    //dealer rejects
    case RejectCard(card) => {
      playerLimbo = false
      playerLayout(pileIndex).sendCard(card)
    }

    //current cards facing on table
    case Table(deck) => {
      //get list of valid card numbers
      if (!playerLimbo) {
        val validCards: List[Int] = Player.playableCards(deck)

        //New hand so reset counter
        pilesWithNoCardToPlay = 0

        breakable {
          for (pile <- playerLayout) {
            //pile has card that can be played
            if (validCards.contains(pile.top()._1)) {
              val cardSubmit: Card = pile.getCard()
              dealer ! SendCard(cardSubmit)
              playerLimbo = true
              pileIndex = playerLayout.indexOf(pile)
              println(playerToString(self) + " sent " + cardToString(cardSubmit) + " to dealer.")

              break
            }
            else pilesWithNoCardToPlay += 1
          }

          if (pilesWithNoCardToPlay == 5) dealer ! PlayerStuck
        }

      }


    }

    //send string repr to dealer
    case CurrentLayoutRequest => {

      dealer ! CurrentLayoutResponse(buildLayoutString())

    }

    //used by dealer to break deadlock/start hand
    case RequestCard => {
      if (playerStack.nonEmpty) {
        sender() ! SendCard(playerStack.head)
        playerStack = playerStack.tail
      }
    }

  }
}
