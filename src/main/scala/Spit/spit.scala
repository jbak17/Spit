package Spit

import akka.actor.{Actor, ActorRef, ActorSystem, Props}

import scala.collection.mutable

/**
  * Created by jeva on 17/04/17.
  */

object spit extends App{

  type Suit = String
  type Card = (Int, Suit)
  type Deck = List[Card] //one or more cards

  //debugging
  case object Children
  case object Parent

  //messages
  case object AskName
  case object CreateChild
  case object SignalChildren
  case object PrintSignal
  case object CurrentLayout
  case object DealCards
  case class AskForCard(current: Deck)
  case class SendCardToPlayer(card: Card)
  case class SendCardsToPlayer(cards: Deck)
  case class RejectCard(card: Card)
  case class BuildLayout(cards: Deck)


  //responses
  case class NameResponse(name: String)
  case class CardResponse(card: Card)
  case class CurrentLayoutResponse(layout: String)

  //creates a shuffled deck of 52 playing cards
  def createDeck(): Deck = {
    val suits: List[Suit] = List("H", "S", "C", "D")
    val deck: Deck = for (
      suit <- suits;
      number <- 1 to 13
    ) yield {
      (number, suit)
    }
    scala.util.Random.shuffle(deck)
  }

  //returns the string representation of a card in form
  // "facevalue suit" without a space, eg. A clubs = AC
  def cardToString(card: Card): String = card match {
    case (10 , _) => "T" + card._2
    case (11, _) => "J" + card._2
    case (12, _) => "Q" + card._2
    case (13, _) => "K" + card._2
    case (1, _) => "A" + card._2
    case (_, _) => card._1.toString + card._2
  }


  /*
   Dealer has custody of the piles during gameplay
   dealer receives cards from players and adjudicates disputes
   dealer communicates with players
   */
  class Dealer extends Actor(){
    println("Dealer created")
    var pileOne: Deck = List()
    var pileTwo: Deck = List()

    val playerOne: ActorRef = context.actorOf(Props[Player], name = "PlayerOne")
    val playerTwo: ActorRef = context.actorOf(Props[Player], name = "PlayerTwo")

    val initialDeck: Deck = createDeck()
    //sent cards to players


    def receive = {
      case Children => {
        println(context.children)
        for (i <- context.children) i ! Children
      }
      case DealCards => {
        println("Dealing...")
        playerOne ! SendCardsToPlayer(initialDeck.take(26))
        playerTwo ! SendCardsToPlayer(initialDeck.takeRight(26))
      }
    }
    //end of dealer
  }

  /*
  Players have their deck and layout
  Players communicate with the dealer and layout
   */
  class Player extends Actor(){
    println("Player created")

    val Layout: ActorRef = context.actorOf(Props[Layout], name = "Layout")

    var playerStack: Deck = List()

    def receive = {
      case Children => println(context.children)
      // forward cards to Layout
      case SendCardsToPlayer(cards) => Layout.forward(SendCardsToPlayer(cards))
    }

  }

  /*
  Layout contains the five piles making the layout
  The layout communicates with the player
  The layout can make changes to the layout without guidance from the player
   */
  class Layout extends Actor(){
    println("Layout created")
    var LayoutPileOne: Deck = List()
    var LayoutPileTwo: Deck = List()
    var LayoutPileThree: Deck = List()
    var LayoutPileFour: Deck = List()
    var LayoutPileFive: Deck = List()

    var playerLayout: List[Deck] = List(LayoutPileOne, LayoutPileTwo, LayoutPileThree, LayoutPileFour, LayoutPileFive)

    /*
    Prints current layout with top card showing and remaining
    cards in stack represented by periods.
    Eg, a starting layout: C3 C2. D9.. HK... SQ....
    */
    def currentLayout(): String = ???

    def receive = {
      case PrintSignal => println(self)
      case CurrentLayout => currentLayout()
      case SendCardsToPlayer(cards) => {
        println("Layout has " + cards.size)
      }
      case BuildLayout => ???


    }
  }


  /*
        ACTOR SYSTEM SETUP
   */
  val system = ActorSystem("Spit_System")
  //create dealer
  val dealer = system.actorOf(Props[Dealer],"Dealer")
  dealer ! DealCards
  dealer ! Children


  Thread.sleep(6000) //giving 6s for all actors to finish work
  system.terminate()
}
