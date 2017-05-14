import Spit.CardPile
import Spit.spit.Card

var pile: CardPile = new CardPile(2)

val c1: Card = (1, "S")
val c2: Card = (2, "S")
val c3: Card = (3, "S")

var lst: List[Int] = List()

lst = 2 :: lst
3 :: lst

pile.sendCard(c1)
pile.size()
pile.sendCard(c2)
pile.size()
pile.sendCard(c3)
pile.size()
pile.max






