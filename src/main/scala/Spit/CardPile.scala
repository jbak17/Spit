package Spit

import Spit.spit.{Card, Deck}

/**
  * Created by jeva on 14/05/17.
  */
class CardPile(max_size: Int) {
  val emptyCard: Card = (0, "X")
  val max: Int = max_size
  var cards: List[Card] = List()

  def size(): Int = cards.length

  //Number of top card, used to check if card can be played.
  def top(): Card = if (cards.nonEmpty) cards.head else emptyCard

  //returns tuple of current card and number of necessary dots. Order on max
  def status(): (Card, Int, Int) = {
    if (cards.nonEmpty) (cards.head, cards.tail.size, max)
    else (emptyCard, 0, max)
  }

  def getMax(): Int = max

  def isEmpty(): Boolean = cards.isEmpty

  //cardPile has full complement
  def isFull(): Boolean = cards.length == max

  //CardPile contains playable card
  def hasValid(valid: List[Int]): Boolean = {
    valid.contains(cards.head)
  }

  //card from pile to player
  def getCard(): Card = {
      val ret = cards.head
      cards = cards.tail
      ret
  }

  //card from player to pile
  def sendCard(card: Card): Unit =
    if (cards.length < max) cards = card :: cards

}
