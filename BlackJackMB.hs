module BlackJack where
    import Cards
    import RunGame
    import Test.QuickCheck
    import Data.List
    
    david = Card King Hearts
    death = Card Ace Spades
    hand = Add (Card (Numeric 2) Hearts)(Add (Card Jack Spades) Empty)
    hand2 = Add death hand
    
    
    {- | Calc size of a hand
    sizeSteps :: [Integer]
    sizeSteps  = [size hand2
                , size (Add (Card (Numeric 2) Hearts)
                            (Add (Card Jack Spades) Empty))
                , 0 + 1 + 1
                ,2]
    -}
    
    
    -- | Used to display a hand as strings
    display :: Hand -> String
    display Empty = ""
    display (Add card hand) 
         | size hand == 0 = displayCard card ++ "\n"
         | otherwise = (displayCard card) ++ ", " ++(display hand) 
    
    -- | Used for testing that display function behave as intended
    prop_display :: Hand -> Bool
    prop_display Empty = display Empty == ""

    
    -- | Used to display a card as a string
    displayCard :: Card -> String
    displayCard (Card (Numeric i) suit) = show i ++ " of " ++ show suit
    displayCard (Card rank suit) = show rank ++ " of " ++ show suit 

    -- | Used for testing that display function behave as intended
    prop_displayCard :: Card -> Bool
    prop_displayCard (Card (Numeric i) suit) = displayCard (Card (Numeric i) suit) == show i ++ " of " ++ show suit
    prop_displayCard (Card rank suit) = displayCard (Card rank suit) == show rank ++ " of " ++  show suit   -- pointless test?? 
    

    -- | get the value of a given hand
    value :: Hand -> Integer
    value hand = initialValue hand - (numberOfAces hand * 10)
  
    -- | get the iniial value of a given hand
    initialValue :: Hand -> Integer
    initialValue Empty = 0
    initialValue (Add card hand) = valueRank (rank card) + value hand 

    -- | get the value of given rank
    valueRank :: Rank -> Integer
    valueRank r | r == King || r == Queen || r == Jack = 10
    valueRank Ace = 11
    valueRank (Numeric i) = i
    
    -- | Calculates number of Aces in a given hand
    numberOfAces :: Hand -> Integer
    numberOfAces Empty = 0
    numberOfAces (Add card hand) = if (rank card == Ace) then 1 + numberOfAces hand else 0 + numberOfAces hand 
    