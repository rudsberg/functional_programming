module Expr where 
    import Data.List

    data Expr = Num Double 
            | MonoOp String (Double -> Double) Expr 
            | BinOp String (Double -> Double -> Double) Expr Expr
            | Var
    
    instance Show Expr 
        where show = showExpr

    showExpr :: Expr -> String
    showExpr (Num n)                  = show n
    showExpr (Var)                    = "x"
    showExpr (MonoOp name _ exp)      = name ++ " " ++ case exp of
                                                       (Num n)   -> showExpr exp
                                                       Var       -> showExpr exp
                                                       _ -> "(" ++ showExpr exp ++ ")" 
    showExpr (BinOp name _ exp1 exp2) = case name of
                                        "+"       -> showExpr exp1 ++ name ++ showExpr exp2
                                        _ -> showFactor exp1 ++ name ++ showFactor exp2
        where showFactor (BinOp name op exp1 exp2) = case name of 
                                                "+" -> "(" ++ showExpr exp1 ++ name ++ showExpr exp2 ++ ")"
                                                _ -> showExpr (BinOp name op exp1 exp2)
    
        
                                --case name of 
                                  --      "+"       -> "(" ++ showExpr exp1 ++ sp ++ name ++ sp ++ showExpr exp2 ++ ")"
                                    --    otherwise -> showExpr exp1 ++ sp ++ name ++ sp ++ showExpr exp2

        {- 
        showExpr (Add e e') = 
            showExpr e ++ " + " ++ showExpr e'
          showExpr (Mul e e') = 
            showFactor e ++ " * " ++ showFactor e'
            where 
              showFactor (Add e1 e2) = "(" ++ showExpr (Add e1 e2) ++ ")"
              showFactor e           = showExpr e -}
    
    x :: Expr
    x = Var

    num :: Double -> Expr
    num d = Num d

    sin, cos :: Expr -> Expr
    sin exp = MonoOp "sin" Prelude.sin exp
    cos exp = MonoOp "cos" Prelude.cos exp 

    add, mul :: Expr -> Expr -> Expr
    add exp1 exp2 = BinOp "+" (+) exp1 exp2
    mul exp1 exp2 = BinOp "*" (*) exp1 exp2

    