package Spit

import Spit.spit._
import akka.actor.{Actor, ActorRef, Props}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

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


class Player extends Actor(){
  println("Player created")

  var playerStack: Deck = List()
  var playerLayout: List[CardPile] = List()

  val cardsToWin = 15

  //number of cards accepted by dealer
  var cardsAccepted: Int = 0

  //keep track of piles that can play, used to prevent deadlock.
  var pilesWithNoCardToPlay: Int = 0


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
      if (playerStack.size > 15){
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
    case AcceptCard =>{}

    //dealer rejects
    case RejectCard =>{}

    //current cards facing on table
    case Table(deck) =>{}

    //send string repr to dealer
    case CurrentLayoutRequest =>{
        var pileStatus: List[(Card, Int, Int)] = List()
        for (pile <- playerLayout){
         pileStatus = pile.status() :: pileStatus
        }
        pileStatus.sortWith(_._3 < _._3) //sorted on third element of tuple
        var outString: List[String] = List()
        for (p <- pileStatus) {
          outString = (cardToString(p._1) + "."*p._2 + " ") :: outString
        }
        dealer ! CurrentLayoutResponse(outString.foldLeft("")(_+_))

    }

    //used by dealer to break deadlock/start hand
    case RequestCard =>{
      if (playerStack.nonEmpty){
        sender() ! SendCard(playerStack.head)
        playerStack = playerStack.tail
      }
    }
    //setup
    /*
    Player receives cards from dealer; builds layout; sends card from player pile.
    @todo manage situation where player cannot form full layout

    case DealCards(cards) => {
      playerStack ++= cards
      Player.buildLayout(playerStack, layoutPiles)
      playerStack = playerStack.takeRight(playerStack.size - 15)
      cardFromDecktoDealer()
      //println("Player deck now has " + playerStack.length + " cards. Should be 10.")

    }

      /*
      Dealer informs player a card has been accepted
      */
    case DealerAcceptedCard => {
      cardsAccepted += 1
      if (cardsAccepted == cardsToWin){
        dealer ! DeclaresVictory
      }
    }

    /*
    Forwards request from dealer to piles
     */
    case CurrentLayoutRequest => for (pile <- layoutPiles) pile ! CurrentPileRequest

    case CurrentGameState(cards) => {
      for (pile <- layoutPiles) pile ! RequestCardFromLayoutPile(Player.playableCards(cards))
      pilesWithNoCardToPlay = 0
    }

    /*
    Processes responses from piles regarding the current state of their
    respective piles.
     */
    case CurrentPileResponse(response: String, size: Int) => {

      val actorIndex = layoutStatus.indexWhere(x => x._1 == sender())
      layoutStatus.updated(actorIndex, (sender(), size))
      if (currentLayout.length < 4) {
        currentLayout += response
        //println(currentLayout)
      }
      else if (currentLayout.length == 4)
      {
        currentLayout += response
        printLayout(currentLayout.toList)
        currentLayout = ListBuffer()
      }
      else println("Something has gone horribly wrong.")
    }

    /*
    Cards received from dealer. Sends cards to piles
    to build layout, and places remaining cards on the stack.
     */
    case SendMultipleCards(cards) => {
      playerStack = cards
      println("player deck size: " + playerStack.size)
      //if player is on final stage
      //@todo: work out how to manage last hand
      if (playerStack.size <= 15) {
        context.parent ! NotifyNoStack
      }
      //build new layout from deck
      else {
        Player.buildLayout(playerStack, layoutPiles)
        playerStack = playerStack.takeRight(playerStack.size - 15)
      }
      println("Player deck now has: " + playerStack.size)
    }

      /*
      Inform dealer that the player cannot play any cards with the current
      dealer layout.
      Used to prevent deadlock
       */
    case NoCardToPlay => {
      //sends request for card to be sent from largest pile
      layoutStatus.sortWith(_._2 > _._2).head._1 ! SendCardToPile(sender())

      pilesWithNoCardToPlay +=1
      if (pilesWithNoCardToPlay == layoutPiles.size)
        context.parent ! PlayerStuck
    }

      /*
    case PileEmpty => {
      emptyPiles += 1
      if (emptyPiles == 5){
        println("Player " + playerToString(self) + " declares victory")
        context.system.terminate()
      }
    }
    */

    /*
    Send card to dealer from stack in reponse to dealer request
     */
    case RequestCardFromPlayerDeck => {
      if (playerStack.nonEmpty){
        sender() ! CardFromPlayerPileToDealerLayout(playerStack.head)
        playerStack = playerStack.tail
        println("Card sent from player deck")
      }
      else println("Player " + playerToString(self) + " deck empty.")

    }
    */
  }//end receive

  /*
   /*
    list of actors and size of their current pile.
    Used to move cards between piles.
     */
   var layoutStatus: List[(ActorRef, Int)] = layoutPiles.zip(1 to 5).toList

   var currentLayout: ListBuffer[String] = ListBuffer()

   /*
 Prints current layout with top card showing and remaining
 cards in stack represented by periods.
 Eg, a starting layout: C3 C2. D9.. HK... SQ....
 */
   def printLayout(layout: List[String]): Unit = println("Current layout for " + playerToString(self) + " is: " + layout.sortBy(x => x.size).foldLeft("")((x,y) => x + y))

   /*
   Sends card from player to dealer if card available on stack.
   @todo If no card available informs dealer stack is empty.
    */
   def cardFromDecktoDealer(): Unit = {
     if (playerStack.length > 0){
       context.parent ! CardFromPlayerPileToDealerLayout(playerStack.head)
       playerStack = playerStack.tail
     }
     //else context.parent ! NoCardToPlay
   }
 */




  //Player ends here
}
