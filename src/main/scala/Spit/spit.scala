package Spit

/**
  * Created by jeva on 17/04/17.
  */

object spit extends App{

  type Suit = String
  type Card = (Int, Suit)
  type Deck = List[Card]

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

}
