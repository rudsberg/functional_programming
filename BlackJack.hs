module BlackJack where
import Cards
import RunGame
import Test.QuickCheck

david = Card King Hearts
death = Card Ace Spades

display :: Hand -> String
display (Add card hand) = undefined

displayCard :: Card -> String
displayCard (Card rank suit) = show rank ++ " of " ++ show suit ++ ", "