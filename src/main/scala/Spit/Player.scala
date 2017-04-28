package Spit

import java.util.NoSuchElementException

import Spit.spit._
import akka.actor.{Actor, ActorRef, Props}

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

/**
  * Created by jeva on 26/04/17.
  */
class Player extends Actor(){
  println("Player created")

  var playerStack: Deck = List()
  val Pile1: ActorRef = context.actorOf(Props[LayoutPile], name = "Pile1")
  val Pile2: ActorRef = context.actorOf(Props[LayoutPile], name = "Pile2")
  val Pile3: ActorRef = context.actorOf(Props[LayoutPile], name = "Pile3")
  val Pile4: ActorRef = context.actorOf(Props[LayoutPile], name = "Pile4")
  val Pile5: ActorRef = context.actorOf(Props[LayoutPile], name = "Pile5")

  val layoutPiles = context.children

  var currentLayout: ListBuffer[String] = ListBuffer()

  @tailrec
  final def buildLayout(cards: Deck, piles: Iterable[ActorRef], index: Int = 0): Unit = {
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

  def receive = {
    case Children => println(context.children)
    /*
    Player receives cards and forwards up to 15 to stack.
    The remaining cards are in playerStack.
     */
    case SendMultipleCards(cards: Deck) => {
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
    case CurrentLayoutRequest => for (pile <- layoutPiles) pile ! CurrentPileRequest
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
  }


}
