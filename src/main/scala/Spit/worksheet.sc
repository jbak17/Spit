import Spit.CardPile
import Spit.spit._

//creates a shuffled deck of 52 playing cards
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

var cards: Deck = createDeck()

val cardsToWin = 15
val cardsAccepted = 12

var pile1: CardPile = new CardPile(1)
var pile2: CardPile = new CardPile(2)
var pile3: CardPile = new CardPile(3)
var pile4: CardPile = new CardPile(4)
var pile5: CardPile = new CardPile(5)

val layoutTemplate: List[CardPile] = List(new CardPile(1), new CardPile(2), new CardPile(3), new CardPile(4), new CardPile(5))

var layout: List[CardPile] = List(pile1, pile2, pile3, pile4, pile5)

for (pile <- layout) {
  for (i <- 1 to pile.max){
    val crd: Card = cards.head
    pile.sendCard(crd)
    cards = cards.tail
  }
}

var layoutWithEmpty: List[CardPile] = layout
layoutWithEmpty.head.getCard()
layoutWithEmpty.head.isEmpty()

def isLayoutBalanced(layout: List[CardPile]): Boolean = {
  !layout.exists(p => p.isEmpty())
}

def countEmpty(layout: Layout): Int = layout.count(p => p.isEmpty())

isLayoutBalanced(layoutWithEmpty)

val crdsRemaining: Int = cardsToWin - cardsAccepted
/*
 Moves cards from full to empty stacks.
  */
def balanceLayout(currentLayout: List[CardPile]): List[CardPile] = {
  //check that there won't always be an empty pile -- near end of hand

  var oldLayout = currentLayout
  //less than 5 cards left but more than one card in a pile
  /*
If there are empty stacks, and there is a stack higher than one => balance

Are there empty stacks: layout.exists(p => p.isEmpty())
Is there a stack higher than one: layout.exists(p => p.size() > 1)

Combining : layout.exists(p => p.isEmpty() & p.size() > 1)
 */

  while (layout.exists(p => p.isEmpty() & p.size() > 1)) {
    //checks if there are empty piles
    //repeats until no empty piles
    val emptyIndex: Int = oldLayout.indexOf(oldLayout.filter(CP => CP.isEmpty()).head)
    //get empty index
    val highIndex: Int = oldLayout.map(CP => (oldLayout.indexOf(CP), CP.size())).sortWith(_._2 > _._2).head._1
    //get highest index
    val swapCard: Card = oldLayout(highIndex).getCard() //card from highest pile
    oldLayout(emptyIndex).sendCard(swapCard) //add card to empty pile
    //Player 1, 7s to empty stack. Layout: 7s Ac 6h.. Qd.. 3c....
  }
  oldLayout
}




var endgameLayout: List[CardPile] = layoutTemplate
print(buildLayoutString(endgameLayout))

val threeCards: Deck = cards.take(3)

(threeCards).foreach(i => endgameLayout(3).sendCard(i))
//newLayout.head.isEmpty()

endgameLayout.exists(p => p.size() > 1)
endgameLayout.exists(p => p.isEmpty())

var newLayout: List[CardPile] = balanceLayout(endgameLayout)

print(buildLayoutString(endgameLayout))

print(buildLayoutString(newLayout))


val i: Int = 1







