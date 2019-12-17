module Expr where 

import Test.QuickCheck
import Parsing
import Data.Maybe
import Data.Char
import Data.List
import Control.Monad

data Expr = Num Double 
  | MonoOp MonoFunc Expr 
  | BinOp BinFunc Expr Expr
  | Var
  deriving (Eq)

data MonoFunc = Sin | Cos
 deriving (Eq, Show)

data BinFunc = Add | Mul
 deriving (Eq, Show) 

instance Show Expr
  where show = showExpr

instance Arbitrary Expr where
   arbitrary = sized arbExpr

showExpr :: Expr -> String
showExpr (Num n)                 = show n 
showExpr Var                     = "x"
showExpr (BinOp Mul e1 e2)       = showFactor e1 ++ " * " ++ showFactor e2
showExpr (BinOp Add e1 e2)       = showExpr e1 ++ " + " ++ showExpr e2
showExpr (MonoOp Sin e)          = "sin "  ++ showArg e
showExpr (MonoOp Cos e)          = "cos " ++ showArg e

showFactor (BinOp Add e1 e2)     = "(" ++ showExpr (add e1 e2) ++ ")"
showFactor e                     = showExpr e                  
showArg (BinOp op e1 e2)         = "(" ++ showExpr (BinOp op e1 e2) ++ ")"
showArg e                        = showExpr e 

--------------------------------------------------------

x :: Expr
x = Var

num :: Double -> Expr
num d = Num d

add,mul :: Expr -> Expr -> Expr
add x y = BinOp Add x y
mul x y = BinOp Mul x y

sin,cos :: Expr -> Expr 
sin x = MonoOp Sin x
cos x = MonoOp Cos x 

----------- TEST DATA ---------------------------
    
test = BinOp Add (Num 1) (Num 2)  -- = 3
testf = MonoOp Sin (Num 0)        -- = 0
test2 = MonoOp Sin test           -- = sin(3) ca 0.14

test3 = mul (add (Num 1) (Num 2)) (add (Num 3) (Num 5))  -- = 3 * 8 = 24
test4 = add (add (Num 1) (Num 2)) (add (Num 3) (Num 5))  -- = 3 + 8 = 11
test5 = mul (mul (Num 1) (Num 2)) (mul (Num 3) (Num 5))  -- = 1*2 * 3*5 = 2 * 15 = 30

test6 = mul (mul (Var) (Num 2)) (mul (Num 3) (Num 5)) -- = 1*2 * 3*5 = 2 * 15 = 30

qFail = MonoOp Cos (MonoOp Cos (BinOp Mul Var (Num (-4.087662660418292))))
failt  = BinOp Mul (MonoOp Cos (BinOp Mul Var (Num 1))) (Num 2)

--------------------------------------------------

-- | Evaluates a given expression, where the second argument is the value of x
eval :: Expr -> Double -> Double
eval  Var x             = x
eval (Num n) _          = n
eval (BinOp Add i j) x  = (eval i x) + (eval j x) 
eval (BinOp Mul i j) x  = (eval i x) * (eval j x)
eval (MonoOp Cos i) x   = Prelude.cos (eval i x)
eval (MonoOp Sin i) x   = Prelude.sin (eval i x)

-- | Tries to read a expression from a string
readExpr :: String -> Maybe Expr
readExpr s = case parse expr s' of
                    Just(ex,"") -> return ex
                    _           -> Nothing
          where s' =  filter(not . isSpace) s


assoc :: Expr -> Expr
assoc (BinOp Add e1 (BinOp Add e2 e3)) = assoc (add (add e1 e2) e3)
assoc (BinOp Add e1 e2)                = add (assoc e1) (assoc e2)  
assoc (BinOp Mul e1 (BinOp Mul e2 e3)) = assoc (mul (mul e1 e2) e3)
assoc (BinOp Mul e1 e2)                = mul (assoc e1) (assoc e2)
assoc (MonoOp op e)                    = MonoOp op (assoc e)
assoc (Num n)                          = Num n
assoc Var                              = Var 
            
-------------PARSERS------------------------
 -- | Parse a number
number :: Parser Expr
number = Num <$> readsP 

-- | Parse an x-variable
variable :: Parser Expr
variable = char 'x' *> return Var

-- | Parse the sin function
sinparse :: Parser Expr
sinparse = do f <- funcparse 's' 'i' 'n' Sin
              e <- factor
              return (MonoOp f e)
-- | Parse the cos function
cosparse :: Parser Expr
cosparse = do f <- funcparse 'c' 'o' 's' Cos
              e <- factor
              return (MonoOp f e)

-- | Simple parser for a three letter function
funcparse a b c f = do c1 <- char a  
                       c2 <- char b 
                       c3 <- char c
                       return f

-- | Parses expressions
expr, term, factor :: Parser Expr
expr   = foldl1 add <$> chain term (char '+')
term   = foldl1 mul <$> chain factor (char '*') 
factor = number <|> variable  <|> char '(' *> expr <* char ')' <|> sinparse <|> cosparse

-- | Test that showExpr and readExpr produces the same result 
prop_ShowReadExpr :: Expr -> Bool
prop_ShowReadExpr e = (assoc $ fromJust $ readExpr (showExpr e)) == assoc e 

-- | Generator for arbitrary expressions
arbExpr :: Int -> Gen Expr
arbExpr n = frequency [(1,rNum),(1,rVar),(n,rBin),(n,rMon)]
    where
      range = 5
      rVar  = return Var
      rNum  =  Num <$> choose (-range,range)
      rBin = do
          let n' = n `div` 2
          op <- oneof [return add, return mul]
          e1 <- arbExpr n'
          e2 <- arbExpr n'
          return $ op e1 e2
      rMon = do
        op <- oneof [return Expr.sin, return Expr.cos]
        e <- arbExpr (n-1)
        return $ op e

-- | Simplifies expressions
simplify :: Expr -> Expr
simplify (Num n)         = Num n
simplify Var           = Var
simplify (BinOp Add x y) = simplifyAdd (simplify x) (simplify y)
simplify (BinOp Mul x y) = simplifyMul (simplify x) (simplify y)
simplify (MonoOp f x)    = simplifyFunc f (simplify x) 

-- | Simplifies multiplication expressions
simplifyMul :: Expr -> Expr -> Expr
simplifyMul (Num 0) e = Num 0
simplifyMul e (Num 0) = Num 0
simplifyMul e (Num 1) = e
simplifyMul (Num 1) e = e
simplifyMul x      y  = mul (simplify x) (simplify y)

-- | Simplifies add expressions
simplifyAdd :: Expr -> Expr -> Expr
simplifyAdd e (Num 0) = simplify e
simplifyAdd (Num 0) e = simplify e
simplifyAdd x       y = add (simplify x) (simplify y)

-- | Simplifies sin and cos expressions
simplifyFunc :: MonoFunc -> Expr -> Expr
simplifyFunc Sin (Num x) | x == 0 ||  x == pi = Num 0
                         | x == (pi/2)        = Num 1
simplifyFunc Sin e                            = MonoOp Sin (simplify e) 
simplifyFunc Cos (Num x) | x == 0 || x == pi  = Num 1
                         | x == (pi/2)        = Num 0
simplifyFunc Cos e                            = MonoOp Cos (simplify e) 

-- | Test that a simplified expression returns correct result
prop_Simplify :: Expr -> Bool
prop_Simplify e = (eval e 0) == (eval (simplify e) 0)

-- | Differentiates an expression with regard to x
differentiate :: Expr -> Expr
differentiate (Num _)                 = Num 0
differentiate (Var)                   = Num 1
differentiate (BinOp Mul Var Var)     = mul (Num 2) Var
differentiate (BinOp Mul e1 e2)       = simplify $ add (mul (differentiate e1) e2) (mul e1 (differentiate e2))
differentiate (BinOp Add e1 e2)       = simplify $ add (differentiate e1) (differentiate e2)
differentiate (MonoOp Sin e)          = simplify $ mul (differentiate e) (MonoOp Cos e)
differentiate (MonoOp Cos e)          = simplify $ mul (Num (-1)) (mul (differentiate e) (MonoOp Sin e))
