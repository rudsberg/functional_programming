import Test.QuickCheck
import Data.Maybe
import Data.List


data Expr = Num Double                              
            | Var                                   
            | BinOp String (Double -> Double -> Double) Expr Expr 
            | MonoOp String (Double -> Double) Expr   


instance Show Expr
    where show = showExpr

showExpr :: Expr -> String
showExpr (Num n)                     = show n 
showExpr Var                         = "x"
showExpr (MonoOp name _ expr)        = name ++ " " ++ case expr of
                        (Num n) -> showExpr expr
                        Var     -> showExpr expr
                        _       -> " (" ++ showExpr expr ++ ") " 
showExpr (BinOp name _ expr1 expr2)  = case name of
    "*" -> showFactor expr1 ++ " " ++ name ++ " " ++ showFactor expr2
    "+" -> showExpr expr1 ++ " " ++ name ++ " " ++ showExpr expr2
    where 
      showFactor (BinOp "+" _ expr1 expr2) = "(" ++ showExpr (add expr1 expr2) ++ ")"
      showFactor expr                      = showExpr expr


        {-}
            add -> case expr2 of 
                    mul -> "(" ++ showExpr expr1 ++ ") " ++ name ++ " " ++ showExpr expr2
            mul -> case expr2 of    
                    add -> showExpr expr1 ++ " " ++ name ++ " (" ++ showExpr expr2 ++ ")"
                    -}



----------- TEST DATA ---------------------------
    
test = BinOp "+" (+) (Num 1) (Num 2)
testf = MonoOp "sin" Prelude.sin (Num 0)
test2 = MonoOp "sin" Prelude.sin test

test3 = mul (add (Num 1) (Num 2)) (add (Num 3) (Num 5))
test4 = add (add (Num 1) (Num 2)) (add (Num 3) (Num 5))
test5 = mul (mul (Num 1) (Num 2)) (mul (Num 3) (Num 5))

--------------------------------------------------

x :: Expr
x = Var

num :: Double -> Expr
num d = Num d

add,mul :: Expr -> Expr -> Expr
add x y = BinOp "+" (+) x y
mul x y = BinOp "*" (*) x y

sin,cos :: Expr -> Expr 
sin x = MonoOp "sin" Prelude.sin x
cos x = MonoOp "cos" Prelude.cos x 

eval :: Expr -> Double
eval (Num x)         = x
eval (BinOp _ o x y) = (eval x) `o` (eval y) 
eval (MonoOp _ o x)  = o (eval x)
