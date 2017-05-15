package Spit

import Spit.spit._
import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import akka.testkit.{DefaultTimeout, ImplicitSender, TestActors, TestKit}
import org.scalatest.FunSuite
import org.scalatest._
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.BeforeAndAfterAll
import org.scalatest.WordSpecLike
import org.scalatest.Matchers
import com.typesafe.config.ConfigFactory

import scala.concurrent.duration._
import scala.collection.immutable
import scala.util.Random


/**
  * Created by jeva on 17/04/17.
  */
class spit$Test extends FlatSpec with ScalaFutures {


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

  def buildLayoutString(playerLayout: List[CardPile]): String = {
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
    "layout: " + outString.foldLeft("")(_ + _)
  }


  //helper variables
  val deck: Deck = createDeck()
  val face_card: Card = (11, "S")
  val number_card: Card = (9, "A")
  val pile2: CardPile = new CardPile(2)
  val layoutFull: List[CardPile] = buildLayout(deck)

  var unbalancedLayout: List[CardPile] = layoutFull
  unbalancedLayout(0).getCard()

  "A deck" should "have 52 cards" in {
    val deck: Deck = createDeck()
    assertResult(true)(deck.length == 52)
  }

  "Two decks" should "have different ordering after dealing" in {
    val d1 = createDeck()
    val d2 = createDeck()
    assert(d1.zip(d2).exists(card => card._1 != card._2))

  }

  "A face card" should "have a string signature implying its standard usage" in {
    assertResult(true)(spit.cardToString(face_card) == "JS")
  }

  "A number card" should "have a string signature implying its standard usage" in {
    assertResult(true)(spit.cardToString(number_card) == "9A")
  }

  "A card of value 5" should "have valid additions of 4 and 6" in {
    assertResult(List(6, 4))(spit.cardToValidNumber((5, "C")))
  }

  "A card of value 1" should "have valid additions of 13 and 2" in {
    assertResult(List(13, 2))(spit.cardToValidNumber((1, "C")))
  }

  "A card of value 13" should "have valid additions of 1 and 12" in {
    assertResult(List(12, 1))(spit.cardToValidNumber((13, "C")))
  }

  "A pile " should " accept cards" in {
    pile2.sendCard(face_card)
    assert(pile2.isEmpty() == false)
  }

  "A pile " should "have a limit" in {
    pile2.sendCard(face_card)
    pile2.sendCard(number_card)
    assert(pile2.isFull() == true)
  }

  "A pile " should "return a card" in {
    pile2.sendCard(face_card)
    pile2.sendCard(number_card)
    val crd: Card = pile2.getCard()
    assert(crd.isInstanceOf[Card] == true)
  }

  "A pile " should "return a card and have fewer cards afterwards" in {
    pile2.sendCard(face_card)
    pile2.sendCard(number_card)
    val start = pile2.size()
    val crd: Card = pile2.getCard()
    val end = pile2.size()
    assertResult(false)(start == end)
  }

  "A layout" should "be non-balanced with an empty cardpile" in {
    assert(false == Player.isLayoutBalanced(unbalancedLayout))
    assert(true == Player.isLayoutBalanced(layoutFull))
  }


  /*

  "A player" should "be able to receive a card from the dealer" in {
    ???
  }
  */

}

/*
class TestKitUsageSpec
  extends TestKit(ActorSystem(
    "TestKitUsageSpec",
    ConfigFactory.parseString(TestKitUsageSpec.config)))
    with DefaultTimeout with ImplicitSender
    with WordSpecLike with Matchers with BeforeAndAfterAll {
  import TestKitUsageSpec._

  val echoRef = system.actorOf(TestActors.echoActorProps)
  val forwardRef = system.actorOf(Props(classOf[ForwardingActor], testActor))
  val filterRef = system.actorOf(Props(classOf[FilteringActor], testActor))

  val dealer = system.actorOf(Props(classOf[Dealer], testActor))
  val player = system.actorOf(Props(classOf[Player], testActor))
  val layoutPile = system.actorOf(Props(classOf[LayoutPile], testActor))

  val randomHead = Random.nextInt(6)
  val randomTail = Random.nextInt(10)
  val headList = immutable.Seq().padTo(randomHead, "0")
  val tailList = immutable.Seq().padTo(randomTail, "1")
  val seqRef =
    system.actorOf(Props(classOf[SequencingActor], testActor, headList, tailList))

  override def afterAll {
    shutdown()
  }

  "A layoutPile" should {"send a card a string representing the layout upon request" in {
    within(500 millis) {
      layoutPile ! CurrentPileRequest
      expectMsg(CurrentLayoutResponse)
      }
    }
  }

  "An EchoActor" should {
    "Respond with the same message it receives" in {
      within(500 millis) {
        echoRef ! "test"
        expectMsg("test")
      }
    }
  }
  "A ForwardingActor" should {
    "Forward a message it receives" in {
      within(500 millis) {
        forwardRef ! "test"
        expectMsg("test")
      }
    }
  }
  "A FilteringActor" should {
    "Filter all messages, except expected messagetypes it receives" in {
      var messages = Seq[String]()
      within(500 millis) {
        filterRef ! "test"
        expectMsg("test")
        filterRef ! 1
        expectNoMsg
        filterRef ! "some"
        filterRef ! "more"
        filterRef ! 1
        filterRef ! "text"
        filterRef ! 1

        receiveWhile(500 millis) {
          case msg: String => messages = msg +: messages
        }
      }
      messages.length should be(3)
      messages.reverse should be(Seq("some", "more", "text"))
    }
  }
  "A SequencingActor" should {
    "receive an interesting message at some point " in {
      within(500 millis) {
        ignoreMsg {
          case msg: String => msg != "something"
        }
        seqRef ! "something"
        expectMsg("something")
        ignoreMsg {
          case msg: String => msg == "1"
        }
        expectNoMsg
        ignoreNoMsg
      }
    }
  }
}

object TestKitUsageSpec {
  // Define your test specific configuration here
  val config =
    akka {
      loglevel = "WARNING"
    }
    """

  /**
    * An Actor that forwards every message to a next Actor
    */
  class ForwardingActor(next: ActorRef) extends Actor {
    def receive = {
      case msg => next ! msg
    }
  }

  /**
    * An Actor that only forwards certain messages to a next Actor
    */
  class FilteringActor(next: ActorRef) extends Actor {
    def receive = {
      case msg: String => next ! msg
      case _           => None
    }
  }

  /**
    * An actor that sends a sequence of messages with a random head list, an
    * interesting value and a random tail list. The idea is that you would
    * like to test that the interesting value is received and that you cant
    * be bothered with the rest
    */
  class SequencingActor(next: ActorRef, head: immutable.Seq[String],
                        tail: immutable.Seq[String]) extends Actor {
    def receive = {
      case msg => {
        head foreach { next ! _ }
        next ! msg
        tail foreach { next ! _ }
      }
    }
  }
}
*/

