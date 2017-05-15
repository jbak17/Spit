import Spit.CardPile
import Spit.spit.{Card, Deck, cardToString, playerToString}

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
val cardsAccepted = 0

var pile1: CardPile = new CardPile(1)
var pile2: CardPile = new CardPile(2)
var pile3: CardPile = new CardPile(3)
var pile4: CardPile = new CardPile(4)
var pile5: CardPile = new CardPile(5)

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

isLayoutBalanced(layoutWithEmpty)


def balanceLayout(currentLayout: List[CardPile]): List[CardPile] = {
  //check that there won't always be an empty pile -- near end of hand
  if (!(cardsToWin - cardsAccepted <= 5)) {
    println("cleared cards to win")
    var oldLayout = currentLayout
    //checks if there are empty piles
    //repeats until no empty piles
    while (!isLayoutBalanced(oldLayout)) {
      println("entered while")
      val emptyIndex: Int = oldLayout.indexOf(oldLayout.filter(CP => CP.isEmpty()).take(1)) //get empty index
      println("empty index {}", emptyIndex)
      val highIndex: Int = oldLayout.map(CP => (oldLayout.indexOf(CP), CP.size())).sortWith(_._2 > _._2).take(1)(0)._1 //get highest index
      val swapCard: Card = oldLayout(highIndex).getCard() //card from highest pile
      oldLayout(emptyIndex).sendCard(swapCard) //add card to empty pile
    }
    oldLayout
  }
  else currentLayout
}

var newLayout: List[CardPile] = balanceLayout(layoutWithEmpty)
newLayout.head.isEmpty()
print(buildLayoutString(newLayout))


val i: Int = 1







