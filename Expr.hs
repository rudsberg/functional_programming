module Expr where 
    import Data.List
    import Parsing


    -- A -- 
    data Expr = Num Double 
            | MonoOp MonoFunc Expr 
            | BinOp BinFunc Expr Expr
            | Var Char
            deriving (Eq, Show)

    data MonoFunc = Sin | Cos
        deriving (Eq, Show)
        
    data BinFunc = Add | Mul
        deriving (Eq, Show)

    x :: Expr
    x = Var 'x'
    num :: Double -> Expr
    num d = Num d
    sin, cos :: Expr -> Expr
    sin exp = MonoOp Sin exp
    cos exp = MonoOp Cos exp 
    add, mul :: Expr -> Expr -> Expr
    add exp1 exp2 = BinOp Add exp1 exp2
    mul exp1 exp2 = BinOp Mul exp1 exp2

    -- Test data -- 
    e1 = add (mul (Num 3) (Var 'x')) (Num 17.3) -- 3*x + 17.3 
    e2 = mul (add (Num 4) (Num 3)) (add (Num 1) (Num 1))  -- (4+3)*(1+1) = 14
    e3 = mul (Num 5) (add (Num 2) (Num 3)) -- 5 * (2+3) = 25
    e4 = mul (Var 'x') (Num 4) -- x*4
    ---------------

    -- B --
    showExpr :: Expr -> String
    showExpr (Num n) = show n 
    showExpr (Var _)   = "x"
    showExpr (MonoOp Sin e) = "sin" ++ showExpr e
    showExpr (MonoOp Cos e) = "cos" ++ showExpr e
    showExpr (BinOp Add e1 e2) = showPlus e1 e2
    showExpr (BinOp Mul e1 e2) = showFactor e1 ++ "*" ++ showFactor e2
    
    showFactor (BinOp Add e1 e2) = "(" ++ showPlus e1 e2 ++ ")"
    showFactor e                 = showExpr e
              
    showPlus e1 e2 = showExpr e1 ++ "+" ++ showExpr e2
    
    -- C --
    eval :: Expr -> Double -> Double
    eval (Num n) _ = n
    eval (Var _) n     = n
    eval (MonoOp Sin e) n = Prelude.sin $ eval e n
    eval (MonoOp Cos e) n = Prelude.cos $ eval e n
    eval (BinOp Add e1 e2) n = eval e1 n + eval e2 n
    eval (BinOp Mul e1 e2) n = eval e1 n * eval e2 n

    -- D -- 
    expr, term, factor :: Parser Expr
    expr = foldl1 add <$> chain term (char '+')
    term = foldl1 mul <$> chain factor (char '*') 
    factor = Num <$> number
        <|> Var <$> char 'x' 
        <|> char '(' *> expr <* char ')'

    readExpr :: String -> Maybe Expr
    readExpr s = do
        (exp, "") <- parse expr s
        return exp

    number :: Parser Double
    number = read <$> oneOrMore digit

    function :: Parser Expr
    function = undefined