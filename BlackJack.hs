module BlackJack where
import Cards
import RunGame
import Test.QuickCheck

-- Testing data
hand2 = Add (Card (Numeric 2) Hearts) (Add (Card Jack Spades) Empty)
david = Card King Hearts
death = Card Ace Spades


-- A1
display :: Hand -> String
display (Add card hand)
    | size (Add card hand) == 0 = ""
    | otherwise                 = (displayCard card) ++ (display hand)

displayCard :: Card -> String
displayCard (Card rank suit) = show rank ++ " of " ++ show suit ++ ", "