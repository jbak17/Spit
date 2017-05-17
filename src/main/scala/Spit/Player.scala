package Spit

import Spit.spit._
import akka.actor.{Actor, ActorLogging}

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
  Checks if there any empty piles in layout.
 */
  def isLayoutBalanced(layout: List[CardPile]): Boolean = {
    !layout.exists(p => p.isEmpty())
  }

}


class Player extends Actor  with ActorLogging {
  log.debug("Player created")

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

  def buildDeckString(): String = playerToString(self) + " has " + playerStack.size + " in their deck"
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
    //check that there won't always be an empty pile -- near end of hand
    log.debug("{} balancing layout", playerToString(self))
    if (!(cardsToWin - cardsAccepted <= 5)) {
      var oldLayout = currentLayout
      //checks if there are empty piles
      //repeats until no empty piles
      while (!Player.isLayoutBalanced(oldLayout)) {
        val emptyIndex: Int = oldLayout.indexOf(oldLayout.filter(CP => CP.isEmpty()).head) //get empty index
        val highIndex: Int = oldLayout.map(CP => (oldLayout.indexOf(CP), CP.size())).sortWith(_._2 > _._2).head._1 //get highest index
        val swapCard: Card = oldLayout(highIndex).getCard() //card from highest pile
        oldLayout(emptyIndex).sendCard(swapCard) //add card to empty pile
        //Player 1, 7s to empty stack. Layout: 7s Ac 6h.. Qd.. 3c....
        println(s"${playerToString(self)}, ${cardToString(swapCard)} to empty stack. ${buildLayoutString(oldLayout)}" )
      }
      oldLayout
    }
    else currentLayout
  }

  def receive = {

    /*
    Dealer sending card to player.
     */
    case SendCard(card, str) => {
      playerStack = card :: playerStack
      log.debug(playerToString(self) + " received {}", cardToString(card))
    }

    /*
    Player to deal with
     */
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
        playerStack = List()
        dealer ! Endgame
        log.debug(playerToString(self) + " has entered endgame")
      }

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
      playerLimbo = false
      playerLayout(pileIndex).sendCard(card)
      dealer ! Sync
      log.debug(playerToString(self) + "player active")
      log.debug(playerToString(self) + "sent sync request")
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
              dealer ! SendCard(cardSubmit, buildLayoutString(playerLayout))
              playerLimbo = true
              pileIndex = playerLayout.indexOf(pile)
              //println(playerToString(self) + " sent " + cardToString(cardSubmit) + " to dealer.")
              log.debug(s"{} sent {} to dealer.", playerToString(self), cardToString(cardSubmit))
              break
            }
            else pilesWithNoCardToPlay += 1
          }

          if (pilesWithNoCardToPlay == 5) dealer ! PlayerStuck
        }

      }
      else if (playerLimbo) log.debug(playerToString(self) + " paused: waiting on dealer to consider submitted card.")



    }

    //send string repr to dealer
    case CurrentLayoutRequest => {

      dealer ! CurrentLayoutResponse(buildLayoutString(playerLayout))

    }

    //used by dealer to break deadlock/start hand
    case RequestCard => {
      if (playerStack.nonEmpty) {
        sender() ! SendCard(playerStack.head)
        playerStack = playerStack.tail
        log.debug(s"{} sent card on request of dealer.", playerToString(self))
        playerLimbo = false
      } else sender() ! SendCard(Player.emptyCard)
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
