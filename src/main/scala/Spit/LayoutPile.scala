
/*



  /*
  Considers whether either of the cards on the table piles can be added to with the card on the top of this pile.
  If card exists send to dealer.
  Card goes into limbo until dealer confirms acceptance.
  If dealer rejects, card is added back to pile.
   */
  def gameStateResponse(cards: List[Int]) = {

    //pile has cards and isn't waiting for dealer response to previous send
    if (pile.nonEmpty & cardLimbo != true){
      if (cards.contains(pile.head._1))
      {
        dealer ! SendSingleCard(pile.head)
        cardLimbo = true
        println("Layout from " + playerToString(context.parent) + " sent " + cardToString(pile.head) + " to dealer.")
      }
      //informs player that pile is stuck with current layout
      else context.parent ! NoCardToPlay
    }
    //pile muse wait until dealer responds
    else if (cardLimbo) {
      Thread.sleep(200)
      context.parent ! PileEmpty
    }
    //pile has no card to play inform player that it needs a card from another pile
    else if (pile.isEmpty) {
      dead = true
      context.parent ! PileEmpty
    }
    else println("Pile empty")
  }


  def receive = {
    //printing
    case CurrentPileRequest => {
      //println(currentPile())
      sender() ! CurrentPileResponse(currentPile(), pile.length)
    }

    //card movements
    case SendSingleCard(card) => {
      pile = pile :+ card
      //println(currentPile())
    }

    /*
    Used to respond to request from player to provide a card to another pile
     */
    case SendCardToPile(sndr) => {
      if (pile.nonEmpty & !cardLimbo) {
        sndr ! MoveBetweenPiles(pile.head)
        pile.remove(0)
      }
      else sender() ! NoCardAvailableFor(sndr)
    }

    /*
    Used for when a pile receives a card from another pile
     */
    case MoveBetweenPiles(card) => {
      pile = pile :+ card
    }

    // game play updates
    case RequestCardFromLayoutPile(cards) => gameStateResponse(cards)

    case RejectCard => {
      println("Dealer rejected " + cardToString(pile.head))
      cardLimbo = false

    }
    case AcceptCard =>{
      if (pile.nonEmpty) pile = pile.tail
      else context.parent ! PileEmpty
    }

  }



}
*/