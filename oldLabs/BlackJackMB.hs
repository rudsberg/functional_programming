module BlackJack where
    import Cards
    import RunGame
    import Test.QuickCheck
    import System.Random
    import Data.List

    -- Test values 
    david = Card King Hearts
    death = Card Ace Spades
    hand = Add (Card (Numeric 2) Hearts)(Add (Card Jack Spades) Empty)
    hand2 = Add death hand
    hand3 = Add david hand2
    deck = fullDeck

    ------------------- 2A --------------------
    
    
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
    prop_display (Add _ hand) = and [displayCard x `isInfixOf` display hand | x <- handAsCardList hand]
   
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
    value hand = if (resultVal <= 21) then resultVal else resultVal - (numberOfAces hand * 10)
        where resultVal = initialValue hand
  
    -- | get the initial value of a given hand
    initialValue :: Hand -> Integer
    initialValue Empty           = 0
    initialValue (Add card hand) = valueRank (rank card) + initialValue hand 

    -- | get the value of given rank
    valueRank :: Rank -> Integer
    valueRank Ace         = 11
    valueRank (Numeric i) = i
    valueRank _           = 10
    
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
    winner guest bank | gameOver guest           = Bank
                      | gameOver bank            = Guest 
                      | value guest > value bank = Guest
                      | otherwise                = Bank

    ------------------ 2B ------------------------



    -- | Given to hands, put the first one on top of the other
    (<+) :: Hand -> Hand -> Hand
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


    -- | Given a deck and a hand, draw a card from the deck and put it on the hand
    draw :: Hand -> Hand -> (Hand,Hand)
    draw Empty _ = error "Can not draw a card from an empty deck!"
    draw (Add card deck) hand = (deck, Add card hand) 

    
    -- | Used to play a bank round
    playBank :: Hand -> Hand
    playBank deck = playBankHelper deck Empty

    -- | Helper function to draw as many card as needed when bank is playing
    playBankHelper :: Hand -> Hand -> Hand
    playBankHelper deck hand = if value biggerHand >= 16 then biggerHand else playBankHelper smallerDeck biggerHand 
         where (smallerDeck,biggerHand) = draw deck hand


    -- | Used to shuffle the deck
    shuffleDeck :: StdGen -> Hand -> Hand
    shuffleDeck _ Empty   = Empty
    shuffleDeck rand deck = Add pickedCard (shuffleDeck rand' newDeck)
         where (n, rand') = randomR (1, size deck) rand
               (pickedCard, newDeck) = (pick n deck, removeCard n deck)


    -- | Given a number, pick and return that card from the given hand 
    pick :: Int -> Hand -> Card
    pick _ Empty                             = error "Can not pick a card from an empty hand"
    pick n (Add card deck) | size deck == 0  = card 
                           | n == 1          = card             -- 1 is first card in a hand
                           | otherwise       = pick (n-1) deck  

    -- | Given a number, remove that card from the hand and return the deck
    removeCard :: Int -> Hand -> Hand
    removeCard _ Empty                               = error "Can not remove a card from an empty deck"
    removeCard n (Add card deck)  | n < 1 || n > 52  = Empty -- Can not remove a card that is not in the deck
                                  | n == 1           = deck   -- 1 is first card in a hand
                                  | otherwise        = (Add card savedHand) <+ (removeCard (n-1) deck)
            where savedHand = Empty


    -- | Test if all cards is still in deck after shuffle
    prop_shuffle_sameCards :: StdGen -> Card -> Hand -> Bool
    prop_shuffle_sameCards g c h = c `belongsTo` h == c `belongsTo` shuffleDeck g h

    -- | Helper function used to check if a card belongs to a hand
    belongsTo :: Card -> Hand -> Bool
    c `belongsTo` Empty = False 
    c `belongsTo` (Add c' h) = c == c' || c `belongsTo` h

    -- | Used to check if size of a hand is preserved after shuffle
    prop_size_shuffle :: StdGen -> Hand -> Bool
    prop_size_shuffle rand deck = size deck == size (shuffleDeck rand deck) 


    implementation = Interface {
      iFullDeck = fullDeck,
      iValue = value,
      iDisplay = display,
      iGameOver = gameOver,
      iWinner = winner,
      iDraw = draw,
      iPlayBank = playBank,
      iShuffle = shuffleDeck
    }

    main :: IO ()
    main = runGame implementation