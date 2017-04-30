package Spit

import java.util.NoSuchElementException

import Spit.spit._
import akka.actor.{Actor, ActorRef, Props}

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

/*
Players have their deck of remaining cards and
five piles of cards representing the layout.
Players communicate with the dealer and cardpiles
 */


class Player extends Actor(){
  println("Player created")

  var playerStack: CardPile = ListBuffer()

  val Pile1: ActorRef = context.actorOf(Props[LayoutPile], name = "Pile1")
  val Pile2: ActorRef = context.actorOf(Props[LayoutPile], name = "Pile2")
  val Pile3: ActorRef = context.actorOf(Props[LayoutPile], name = "Pile3")
  val Pile4: ActorRef = context.actorOf(Props[LayoutPile], name = "Pile4")
  val Pile5: ActorRef = context.actorOf(Props[LayoutPile], name = "Pile5")

  val layoutPiles = context.children

  var currentLayout: ListBuffer[String] = ListBuffer()

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

  /*
  Prints current layout with top card showing and remaining
  cards in stack represented by periods.
  Eg, a starting layout: C3 C2. D9.. HK... SQ....
  */
  def printLayout(layout: List[String]): Unit = println("Current layout is: " + layout.sortBy(x => x.size).foldLeft("")((x,y) => x + y))

  def playableCards(cards: Deck): List[Int] = cards.map(card => cardToValidNumber(card)).flatten

  def receive = {
    //setup
    case DealCards(cards) => {
      playerStack ++= cards
      buildLayout(playerStack, layoutPiles)
      playerStack = playerStack.takeRight(playerStack.size - 15)
      sender() ! CardFromPlayerPile(playerStack.head)
      playerStack.remove(0)
      //println("Player deck now has " + playerStack.length + " cards. Should be 10.")

    }

    //forwards to piles
    case CurrentLayoutRequest => for (pile <- layoutPiles) pile ! CurrentPileRequest
    case CurrentGameState(cards) => for (pile <- layoutPiles) pile ! RequestCardFromLayoutPile(playableCards(cards))

    //printing
    case Children => println(context.children)
    case CurrentPileResponse(response: String) => {

      if (currentLayout.length < 4) {
        currentLayout += response
        //println(currentLayout)
      }
      else if (currentLayout.length == 4)
      {
        currentLayout += response
        printLayout(currentLayout.toList)
      }
      else println("Something has gone horribly wrong.")
    }

    //card movements
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
        buildLayout(playerStack, layoutPiles)
        playerStack = playerStack.takeRight(playerStack.size - 15)
      }
      println("Player deck now has: " + playerStack.size)
    }

  }

//Player ends here
}
