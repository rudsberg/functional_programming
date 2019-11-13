module BlackJack where
    import Cards
    import RunGame
    import Test.QuickCheck
    import Data.List
    
    -- Test values 
    david = Card King Hearts
    death = Card Ace Spades
    hand = Add (Card (Numeric 2) Hearts)(Add (Card Jack Spades) Empty)
    hand2 = Add death hand
    
    
    -- | Calc size of a hand
    sizeSteps :: [Integer]
    sizeSteps  = [size hand
                , size (Add (Card (Numeric 2) Hearts)
                            (Add (Card Jack Spades) Empty))
                , 1 + size(Add(Card Jack Spades)Empty)
                , 1 + 1 + 0
                ,2]
    
    
    -- | Used to display a hand as strings
    display :: Hand -> String
    display Empty = ""
    display (Add card hand) 
         | size hand == 0 = displayCard card ++ "\n"
         | otherwise      = (displayCard card) ++ "\n" ++(display hand) 
    
    -- | Used for testing that display function behave as intended.
    prop_display :: Hand -> Bool
    prop_display Empty = display Empty == ""
    prop_display (Add card hand) = and [displayCard x `isInfixOf` display hand | x <- handAsCardList hand]
   
    -- | Converts a hand into list of cards representing the hand.
    handAsCardList :: Hand -> [Card]
    handAsCardList Empty = []
    handAsCardList (Add card hand)
        | size hand == 0    = card : []
        | otherwise         = handAsCardList hand
         
    
    -- | Used to display a card as a string
    displayCard :: Card -> String
    displayCard (Card (Numeric i) suit) = show i ++ " of " ++ show suit
    displayCard (Card rank suit)        = show rank ++ " of " ++ show suit 
 

    -- | get the value of a given hand
    value :: Hand -> Integer
    value hand = if (resultVal < 21) then resultVal else resultVal - (numberOfAces hand * 10)
        where resultVal = initialValue hand
  
    -- | get the iniial value of a given hand
    initialValue :: Hand -> Integer
    initialValue Empty           = 0
    initialValue (Add card hand) = valueRank (rank card) + value hand 

    -- | get the value of given rank
    valueRank :: Rank -> Integer
    valueRank Ace         = 11
    valueRank (Numeric i) = i
    valueRank r | r == King || r == Queen || r == Jack = 10
    
    -- | Calculates number of Aces in a given hand
    numberOfAces :: Hand -> Integer
    numberOfAces Empty           = 0
    numberOfAces (Add card hand) = if (rank card == Ace) then 1 + numberOfAces hand else 0 + numberOfAces hand 

    maxValue = 21

    -- | Checks if the users score is > 21, used to decide if a user has lost the game
    gameOver :: Hand -> Bool
    gameOver hand = value hand > maxValue

    -- | Used to decide if player or bank has won (order dependent)
    winner :: Hand -> Hand -> Player
    winner guest bank | gameOver guest = Bank
                      | gameOver bank  = Guest -- gameOver guest is already tested
                      | otherwise      = if value guest > value bank then Guest else Bank

    -----------2B----------------------------------------



    -- | Given to hands, put the first one on top of the other
    (<+) :: Hand -> Hand -> Hand
    (<+) Empty Empty = Empty
    (<+) Empty hand = hand
    (<+) hand Empty = hand
    (<+) (Add card hand1) hand2 = Add card (hand1 <+ hand2)

    -- | Used to test associativity of '<+' operator
    prop_onTopOf_assoc :: Hand -> Hand -> Hand -> Bool
    prop_onTopOf_assoc p1 p2 p3 =
        p1<+(p2<+p3) == (p1<+p2)<+p3

    -- | Used to test that size of two combined hands equals the sum of the size of the individual hands
    prop_size_onTopOf :: Hand -> Hand -> Bool
    prop_size_onTopOf hand1 hand2 = (size hand1 + size hand2) == size (hand1 <+ hand2)


    -- | Returns a full deck
    fullDeck :: Hand
    fullDeck = fullSuit Hearts <+ fullSuit Clubs <+ fullSuit Diamonds <+ fullSuit Spades

    -- | Used to create a full suit of cards as a hand
    fullSuit :: Suit -> Hand 
    fullSuit s = cardsToHand (cardList s)

    -- | Return all ranks in one suit. Used to create a new deck
    rankList :: [Rank]
    rankList = [Numeric i | i <- [2..10]] ++ [r | r <- [Jack, Queen, King, Ace]]

    -- | Return all cards belonging to a given suit. Used to create a new deck
    cardList :: Suit -> [Card]
    cardList s = [Card r s | r <- rankList]

    -- | Converts a list of cards to a hand
    cardsToHand :: [Card] -> Hand
    cardsToHand []     = Empty
    cardsToHand (x:xs) = Add x (cardsToHand xs)
