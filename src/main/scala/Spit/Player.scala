package Spit

import java.util.NoSuchElementException

import Spit.Dealer.PlayerStuck
import Spit.LayoutPile.SendCardToPile
import Spit.Player.{CurrentPileResponse, NoCardToPlay, PileEmpty}
import Spit.spit._
import akka.actor.{Actor, ActorRef, Props}

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

/*
Players have their deck of remaining cards and
five piles of cards representing the layout.
Players communicate with the dealer and cardpiles
 */
object Player {

  case object DealCards
  case object NoCardToPlay
  case object PileEmpty
  case object RequestCardFromPlayerDeck
  case class SendMultipleCards(cards: CardPile)
  case class DealCards(cards: Deck)
  case class CurrentGameState(current: Deck)
  case class CurrentPileResponse(pile: String, size: Int)
  case class NoCardAvailableFor(sndr: ActorRef)



  @tailrec
  final def buildLayout(cards: CardPile, piles: Iterable[ActorRef], index: Int = 0): Unit = {
    var counter: Int = 0

    if (index < 5){
      for (pile <- piles) {
        pile ! SendSingleCard(cards(counter))
        counter += 1
      }
      buildLayout(cards.drop(counter), piles.drop(1), index+1)
    }


  }

  def playableCards(cards: Deck): List[Int] = cards.map(card => cardToValidNumber(card)).flatten

}

class Player extends Actor(){
  println("Player created")

  var playerStack: CardPile = ListBuffer()

  /*
  5 layout piles are actors that can deal directly with the dealer
   */
  val Pile1: ActorRef = context.actorOf(Props[LayoutPile], name = "Pile1")
  val Pile2: ActorRef = context.actorOf(Props[LayoutPile], name = "Pile2")
  val Pile3: ActorRef = context.actorOf(Props[LayoutPile], name = "Pile3")
  val Pile4: ActorRef = context.actorOf(Props[LayoutPile], name = "Pile4")
  val Pile5: ActorRef = context.actorOf(Props[LayoutPile], name = "Pile5")

  var layoutPiles = context.children
  /*
   list of actors and size of their current pile.
   Used to move cards between piles.
    */
  var layoutStatus: List[(ActorRef, Int)] = layoutPiles.zip(1 to 5).toList

  var pilesWithNoCardToPlay: Int = 0
  var emptyPiles: Int = 0

  var currentLayout: ListBuffer[String] = ListBuffer()

  /*
Prints current layout with top card showing and remaining
cards in stack represented by periods.
Eg, a starting layout: C3 C2. D9.. HK... SQ....
*/
  def printLayout(layout: List[String]): Unit = println("Current layout for " + playerToString(self) + " is: " + layout.sortBy(x => x.size).foldLeft("")((x,y) => x + y))

  def receive = {
    //setup
    case DealCards(cards) => {
      playerStack ++= cards
      Player.buildLayout(playerStack, layoutPiles)
      playerStack = playerStack.takeRight(playerStack.size - 15)
      sender() ! CardFromPlayerPile(playerStack.head)
      playerStack.remove(0)
      //println("Player deck now has " + playerStack.length + " cards. Should be 10.")

    }

    //forwards to piles
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
      if (pilesWithNoCardToPlay + emptyPiles == layoutPiles.size)
        context.parent ! PlayerStuck
    }

    case PileEmpty => {
      emptyPiles += 1
      if (emptyPiles == 5){
        println("Player " + playerToString(self) + " declares victory")
        context.system.terminate()
      }
    }

    /*
    Send card to dealer from stack in reponse to dealer request
     */
    case RequestCardFromPlayerDeck => {
      if (playerStack.nonEmpty){
        sender() ! CardFromPlayerPile(playerStack.head)
        playerStack = playerStack.tail
        println("Card sent from player deck")
      }
      else println("Player " + playerToString(self) + " deck empty.")

    }
  }//end receive



  //Player ends here
}
