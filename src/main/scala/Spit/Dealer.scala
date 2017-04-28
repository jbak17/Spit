package Spit

import Spit.spit._
import akka.actor.{Actor, ActorRef, Props}

/**
  * Created by jeva on 26/04/17.
  */
class Dealer extends Actor(){
  println("Dealer created")
  var pileOne: Deck = List()
  var pileTwo: Deck = List()

  //create players
  val playerOne: ActorRef = context.actorOf(Props[Player], name = "PlayerOne")
  val playerTwo: ActorRef = context.actorOf(Props[Player], name = "PlayerTwo")

  val players = List(playerOne, playerTwo)
  val initialDeck: Deck = createDeck()
  //sent cards to players


  def receive = {
    case Children => {
      println(context.children)
      for (i <- context.children) i ! Children
    }
    case DealCards => {
      println("Dealing...")
      playerOne ! SendMultipleCards(initialDeck.take(26))
      playerTwo ! SendMultipleCards(initialDeck.takeRight(26))
    }
    case CurrentLayoutRequest => for(i <- players) i ! CurrentLayoutRequest
  }
  //end of dealer

}
