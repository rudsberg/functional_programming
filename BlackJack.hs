module BlackJack where
    import Cards
    import RunGame
    import Test.QuickCheck
    import Data.List
    
    -- Hands for testing 
    david = Card King Hearts
    death = Card Ace Spades
    hand = Add (Card (Numeric 2) Hearts)(Add (Card Jack Spades) Empty)
    hand2 = Add death hand
    hand3 = Add (Card Ace Diamonds) hand2
    hand4 = Add (Card King Diamonds) hand

    -- | Calc size of a hand
    sizeSteps :: [Integer]
    sizeSteps  = [size hand
                 , size (Add (Card (Numeric 2) Hearts)
                    (Add (Card Jack Spades) Empty))
                 , 1 + size(Add(Card Jack Spades)Empty)
                 , 1 + 1 + 0
                 , 2]

    -- A1
    -- | Displays hand as a String
    display :: Hand -> String
    display Empty = ""
    display (Add card hand) 
         | size hand == 0 = displayCard card ++ "/n"
         | otherwise = displayCard card ++ ", " ++ (display hand) 

    -- | Displays card as a String
    displayCard :: Card -> String
    displayCard (Card (Numeric i) suit) = show i ++ " of " ++ show suit
    displayCard (Card rank suit) = show rank ++ " of " ++ show suit
    
    -- | Used for testing that display function behave as intended.
    prop_display :: Hand -> Bool
    prop_display Empty = display Empty == ""
    prop_display (Add card hand) = and [displayCard x `isInfixOf` display hand | x <- handAsCardList hand]

    -- | Converts a hand into list of cards representing the hand.
    handAsCardList :: Hand -> [Card]
    handAsCardList (Add card hand)
        | size hand == 0    = card : []
        | otherwise         = handAsCardList hand
      
    -- A2
    maxScore :: Integer
    maxScore = 21

    -- | Provides value for a hand.
    value :: Hand -> Integer
    value Empty           = 0
    value hand            = if (result <= maxScore) then result else result - (10 * numberOfAces hand)
        where result      = initialValue hand

    -- | Helper function providing value when aces are scored as 11.
    initialValue :: Hand -> Integer
    initialValue Empty           = 0
    initialValue (Add card hand) = valueRank (rank card) + initialValue hand

    -- | Numeric value of a rank
    valueRank :: Rank -> Integer
    valueRank r | r == King || r == Queen || r == Jack = 10
    valueRank Ace         = 11
    valueRank (Numeric i) = i
    
    -- | Number of aces in a hand
    numberOfAces :: Hand -> Integer
    numberOfAces Empty           = 0
    numberOfAces (Add card hand) = if (rank card == Ace) then 1 + numberOfAces hand else 0 + numberOfAces hand 
    
    -- A3
    -- | If hand is above max allowed score
    gameOver :: Hand -> Bool
    gameOver hand = value hand > 21

    -- A4
    winner :: Hand -> Hand -> Player
    winner guestHand bankHand
        | guestFail                   = Bank
        | guestFail && bankFail       = Bank
        | not(guestFail) && bankFail  = Guest
        | otherwise                   = if (value guestHand > value bankHand) then Guest else Bank
        where bankFail  = gameOver bankHand
              guestFail = gameOver guestHand

    -- B1
    (<+) :: Hand -> Hand -> Hand
    (<+) = undefined