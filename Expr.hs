module Expr where 
    import Data.List
    import Parsing


    -- A -- 
    data Expr = Num Double 
            | MonoOp MonoFunc Expr 
            | BinOp BinFunc Expr Expr
            | Var

    data MonoFunc = Sin | Cos
    data BinFunc = Add | Mul

    x :: Expr
    x = Var

    num :: Double -> Expr
    num d = Num d

    sin, cos :: Expr -> Expr
    sin exp = MonoOp Sin exp
    cos exp = MonoOp Cos exp 

    add, mul :: Expr -> Expr -> Expr
    add exp1 exp2 = BinOp Add exp1 exp2
    mul exp1 exp2 = BinOp Mul exp1 exp2

    -- Test data -- 
    e1 = add (mul (Num 3) (Var)) (Num 17.3) -- 3*x + 17.3
    e2 = mul (add (Num 4) (Num 3)) (add (Num 1) (Num 1))  -- (4+3)*(1+1)
    e3 = mul (Num 5) (add (Num 2) (Num 3)) -- 5 * (2+3)
    e4 = mul (Var) (Num 4) -- x*4
    ---------------

    -- B --
    instance Show Expr 
        where show = showExpr

    showExpr :: Expr -> String
    showExpr (Num n) = show n 
    showExpr Var     = "x"
    showExpr (MonoOp Sin e) = "sin" ++ showExpr e
    showExpr (MonoOp Cos e) = "cos" ++ showExpr e
    showExpr (BinOp Add e1 e2) = showPlus e1 e2
    showExpr (BinOp Mul e1 e2) = showFactor e1 ++ "*" ++ showFactor e2
    
    showFactor (BinOp Add e1 e2) = "(" ++ showPlus e1 e2 ++ ")"
    showFactor e                 = showExpr e
              
    showPlus e1 e2 = showExpr e1 ++ "+" ++ showExpr e2s
        
        
        ----------- TEST DATA ---------------------------
    {-  
    test = BinOp "+" (+) (Num 1) (Num 2)
    testf = MonoOp "sin" Prelude.sin (Num 0)    
    test2 = MonoOp "sin" Prelude.sin test
        
    test3 = mul (add (Num 1) (Num 2)) (add (Num 3) (Num 5))
    test4 = add (add (Num 1) (Num 2)) (add (Num 3) (Num 5))
    test5 = mul (mul (Num 1) (Num 2)) (mul (Num 3) (Num 5)) -} 

        --------------------------------------------------

    eval :: Expr -> Double -> Double
    eval = undefined
    {-
    eval Var x                    = x
    eval (Num n) _                = n
    eval (MonoOp _ op exp) x      = op $ eval exp x
    eval (BinOp _ op exp1 exp2) x = op (eval exp1 x) (eval exp2 x) -}

    -- | Parse a number
    number :: Parser Double
    number = undefined --read <$> oneOrMore digit

    string :: String -> Parser String
    string s = undefined --read <$> oneOrMore 

    operator :: Char -> (Double -> Double -> Double) -> Parser Expr
    operator c op = undefined {-do n1 <- number 
                       n <- read <$> do
                       n2 <- number
                       return (BinOp n op (Num n1) (Num n2))-}

    readExpr :: String -> Maybe Expr
    readExpr s = undefined