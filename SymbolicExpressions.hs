-- | Symbolic Expressons
-- More exercises with recursive data types
-- Functional Programming course 2018.
-- Thomas Hallgren

{-
This started as a skeleton, the definitions were filled in
during the lecture.
-}

module SymbolicExpressions where
    import Data.List(union)
    
    -- | A Haskell data type for arithmetic expressions with variables
    data Expr = Num Integer
              | Var Name
              | Add Expr Expr
              | Mul Expr Expr
              deriving (Eq)
    
    type Name = String
    
    ex1 = Num 2
    ex2 = Add (Num 2) (Num 2)
    ex3 = Mul (Add (Num 1) (Num 2)) (Num 3)
    ex4 = Add (Num 1) (Mul (Num 2) (Num 3))
    ex5 = Add (Mul (Num 2) x) (Mul (Num 3) y)
    ex6 = Add (Mul (Num 2) (Mul x x)) (Mul (Num 3) y)
    
    x   = Var "x"
    y   = Var "y"
    
    --------------------------------------------------------------------------------
    -- * Showing expressions
    
    instance Show Expr where
      show = showExpr
    
    showExpr :: Expr -> String
    showExpr (Add e1 e2) = showExpr e1 ++ "+" ++ showExpr e2
    showExpr e           = showFactor e
    
    showFactor :: Expr -> String
    showFactor (Mul e1 e2) = showFactor e1 ++ "*" ++ showFactor e2
    showFactor (Num n) = show n
    showFactor (Var s) = s
    showFactor e       = "("++showExpr e++")"
    
    --------------------------------------------------------------------------------
    
    -- | Gathering variables
    vars :: Expr -> [Name]
    vars (Num n) = []
    vars (Var x) = [x]
    vars (Add a b) = vars a `union` vars b
    vars (Mul a b) = vars a `union` vars b
    
    
    
    -- | Substituting expressions for variables
    substitute :: [(Name,Expr)] -> Expr -> Expr
    substitute env (Num n) = Num n
    substitute env (Var x) = case lookup x env of
                               Nothing -> error ("Variable not defined: "++x)
                               Just e  -> e
    substitute env (Add a b) = Add (substitute env a) (substitute env b)
    substitute env (Mul a b) = Mul (substitute env a) (substitute env b)
    
    --------------------------------------------------------------------------------
    
    -- | Evaluating Symbolic Expressions
    
    eval :: [(Name,Integer)] -> Expr -> Integer
    eval env (Num n)   = n
    eval env (Var x)   = case lookup x env of
                           Nothing -> error ("Varible not defined: "++x)
                           Just n  -> n
    eval env (Add a b) = eval env a + eval env b
    eval env (Mul a b) = eval env a * eval env b
    
    
    --------------------------------------------------------------------------------
    -- * Symbolic Differentiation
    
    -- | Symbolic Differentiation function
    diff :: Expr -> Name -> Expr
    diff (Num n)   x             = Num 0
    diff (Var y)   x | y==x      = Num 1
                     | otherwise = Num 0
    diff (Add a b) x             = add (diff a x) (diff b x)
    diff (Mul a b) x             = add (mul (diff a x) b) (mul a (diff b x))
    
    
    -- * Simplifying expressions by using "smart" constructors
    
    -- 0+e == e, e+0 == e, 1*e == e, e*1 == e, 0*e == 0, e*0 == 0
    
    add (Num 0) e = e
    add e (Num 0) = e
    add e1 e2     = Add e1 e2
    
    mul (Num 1) e = e
    mul e (Num 1) = e
    mul (Num 0) e = Num 0
    mul e (Num 0) = Num 0
    mul e1 e2     = Mul e1 e2
    