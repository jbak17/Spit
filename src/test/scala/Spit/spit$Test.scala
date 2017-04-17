package Spit

import Spit.spit.{Card, Deck}
import org.scalatest.FunSuite
import org.scalatest._

/**
  * Created by jeva on 17/04/17.
  */
class spit$Test extends FlatSpec {

  //helper variables
  val deck: Deck = spit.createDeck()
  val face_card: Card = (11, "S")
  val number_card: Card = (9, "A")

  "A deck" should "have 52 cards" in {
    val deck: Deck = spit.createDeck()
    assertResult(true)(deck.length == 52)
  }

  "Two decks" should "have different ordering after dealing" in {
    val d1 = spit.createDeck()
    val d2 = spit.createDeck()
    assert(d1.zip(d2).exists(card => card._1 != card._2))

  }

  "A face card" should "have a string signature implying its standard usage" in {
    assertResult(true)(spit.cardToString(face_card) == "JS")
  }

  "A number card" should "have a string signature implying its standard usage" in {
    assertResult(true)(spit.cardToString(number_card) == "9A")
  }

}
