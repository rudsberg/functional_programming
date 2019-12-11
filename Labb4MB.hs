import Test.QuickCheck
import Parsing
import Data.Maybe
import Data.Char
import Data.List
import Control.Monad


data Expr = Num Double                              
            | Var Char                                   
            | BinOp String (Double -> Double -> Double) Expr Expr 
            | MonoOp String (Double -> Double) Expr   


instance Show Expr
    where show = showExpr

showExpr :: Expr -> String
showExpr (Num n)                     = show n 
showExpr (Var _)                     = "x"
showExpr (MonoOp name _ expr)        = name ++ " " ++ case expr of
                        (Num n) -> showExpr expr
                        (Var _) -> showExpr expr
                        _       -> " (" ++ showExpr expr ++ ") " 
showExpr (BinOp name _ expr1 expr2)  = case name of
    "*" -> showFactor expr1 ++ " " ++ name ++ " " ++ showFactor expr2
    "+" -> showExpr expr1 ++ " " ++ name ++ " " ++ showExpr expr2
    where 
      showFactor (BinOp "+" _ expr1 expr2) = "(" ++ showExpr (add expr1 expr2) ++ ")"
      showFactor expr                      = showExpr expr

--------------------------------------------------------

x :: Expr
x = Var 'x'

num :: Double -> Expr
num d = Num d

add,mul :: Expr -> Expr -> Expr
add x y = BinOp "+" (+) x y
mul x y = BinOp "*" (*) x y

sin,cos :: Expr -> Expr 
sin x = MonoOp "sin" Prelude.sin x
cos x = MonoOp "cos" Prelude.cos x 

----------- TEST DATA ---------------------------
    
test = BinOp "+" (+) (Num 1) (Num 2)     -- = 3
testf = MonoOp "sin" Prelude.sin (Num 0) -- = 0
test2 = MonoOp "sin" Prelude.sin test    -- = sin(3) ca 0.14

test3 = mul (add (Num 1) (Num 2)) (add (Num 3) (Num 5)) -- = 3 * 8 = 24
test4 = add (add (Num 1) (Num 2)) (add (Num 3) (Num 5)) -- = 3 + 8 = 11
test5 = mul (mul (Num 1) (Num 2)) (mul (Num 3) (Num 5)) -- = 1*2 * 3*5 = 2 * 15 = 30

test6 = mul (mul (Var 'x') (Num 2)) (mul (Num 3) (Num 5)) -- = 1*2 * 3*5 = 2 * 15 = 30

--------------------------------------------------

-- | Evaluates a given expression, where the second argument is the value of x
eval :: Expr -> Double -> Double
eval (Var _) x          = x
eval (Num n) _          = n
eval (BinOp _ op i j) x = (eval i x) `op` (eval j x) 
eval (MonoOp _ op i) x  = op (eval i x)

-- | Tries to read a expression from a string
readExpr :: String -> Maybe Expr
readExpr s = undefined

{-}
            do  
              let s' = filter (not . isSpace) s
             
              -}

 -- | Parse a number
number :: Parser Double
number =  read <$> oneOrMore digit

-- | Parse an x-variable
xparse :: Parser Char
xparse =  char 'x'

expr, term, factor :: Parser Expr
expr   = foldl1 add <$> chain term (char '+')
term   = foldl1 mul <$> chain factor (char '*')
factor = Num <$> number <|> (Var <$> xparse <|> char '(' *> expr <* char ')')



main = do putStrLn "Welcome to the simple calculator!"
readEvalPrint = do
    putStr "What would you like to calculate?"
    s <- getLine
    let s' = filter (not . isSpace) s
    case parse expr s' of
       Just (e, "") -> print $ eval e 1
       Nothing      -> putStrLn "Invalid Expression!"