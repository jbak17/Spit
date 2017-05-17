Describe, in a document, your design strategy for the game. What are your Actors going to be? How are you going to resolve the race aspects of the game, such as whose hand slaps the deck first?

Project:
An implementation of the card game Spit using Scala and Akka.

Summary:
Spit is a two-player card game based on the speed of the players. To model this game, we are going to two kinds of actors: a dealer and a player.

The
Gameplay: how is this going to work?

Set up:
	DealCards: 
		Dealer deals cards to players; 
		players create layout for themselves;
		Dealer requests a card from each player;
		Dealer adds those cards to initial pile;

	What is actually happening:
	    dealer instatiated and;
	     - creates deck;
	     - two card piles;
	     - two players;
	    player instantiated and:
	     - creates card pile; and
	     - creates 5 layout actors
	    layout pile instantiated and:
	     - creates a buffer for cards

	    dealer sends 26 cards to each player (dealer.dealCards)


	StartGame

The dealer reports what the two cards are;

If a player has an eligible card they send it to the dealer;

If the dealer accepts the card the dealer sends a new message to the players saying what the current card is;

If the dealer gets a card that is ineligible it is returned to sender;

If neither player can pass a card the dealer requests a new card from the deck of each player; 

If a layout is empty it signals the dealer to end the game;
