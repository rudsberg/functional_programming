-- | Parsing
-- Examples to illustrate how to write parsers using parsing combinators
-- Functional Programming course 2018.
-- Thomas Hallgren

{-
This started as a skeleton, the definitions were filled in
during the lecture.
-}
module ParsingExamples where
    import Data.Char(isDigit)
    import Parsing hiding (chain,digit)
    import Control.Monad(forever)
        
    --------------------------------------------------------------------------------
    -- * A first example
    -- Writing a recursive decent parser directly
    -- Using functions of type String -> Maybe (a,String)
    
    {- BNF:
    digit = "0".."9".
    number = digit{digit}.
    addition = number "+" number.
    -}

    type ParserFun a = String -> Maybe (a, String)

    num :: ParserFun Integer
    num s = case span isDigit s of
       (d:ds, rest) -> Just (read (d:ds), rest) -- Only use read if we know it succeeds (not empty and not characters)
       _            -> Nothing

    number_v1 :: String -> Maybe (Integer,String)
    number_v1 s = case span isDigit s of
                    ("",_) -> Nothing
                    (ds,r) -> Just (read ds,r)


    addition0 :: ParserFun Integer
    addition0 s = case num s of 
                     Just (n, '+':r) -> case num r of 
                                          Just (m, r') -> Just (n+m, r')
                                          _            -> Nothing
                     _               -> Nothing

    
    addition_v1 :: String -> Maybe (Integer,String)
    addition_v1 s = case number_v1 s of
                      Just (n1,'+':r1) -> case number_v1 r1 of
                                            Just (n2,r2) -> Just (n1+n2,r2)
                                            _ -> Nothing
                      _ -> Nothing
    
    
    {- A small extension to the BNF
    multiplication ::= number "*" number.
    calculation    ::= addition | multiplication.
    -}
    
    multiplication_v1 :: String -> Maybe (Integer,String)
    multiplication_v1 s = case number_v1 s of
                            Just (n1,'*':r1) -> case number_v1 r1 of
                                                  Just (n2,r2) -> Just (n1*n2,r2)
                                                  _ -> Nothing
                            _ -> Nothing
    
    
    calculation_v1 :: String -> Maybe (Integer,String)
    calculation_v1 s = case addition_v1 s of
                         Nothing -> multiplication_v1 s
                         result  -> result
                     
    
    --------------------------------------------------------------------------------
    -- * Rewriting our first example using parsing combinators
    
    -- | Parse a digit (also available in the Parsing module)
    digit :: Parser Char
    digit = sat isDigit
    
    -- | Parse a number
    number :: Parser Integer
    number = read <$> oneOrMore digit
             --do ds <- oneOrMore digit
             --   return (read ds)
    
    -- | Parse two numbers, separated by +, and add them
    addition :: Parser Integer
    {-
    addition = do n1 <- number
                  char '+'
                  n2 <- number
                  return (n1+n2)     -}


    {-
    addition = do n1 <- number
                  char '+'
                  n2 <- number
                  return (n1+n2)
    -}
    addition = operator '+' (+)
    
    -- | Parse two numbers, separated by *, and multiply them
    multiplication :: Parser Integer
    {-
    multiplication =do n1 <- number
                       char '*'
                       n2 <- number
                       return (n1*n2)
    -}
    multiplication = operator '*' (*)

    operator c op = do n1 <- number 
                       char c 
                       n2 <- number
                       return (n1 `op` n2)
    
    
    calculation :: Parser Integer
    calculation = addition <|> multiplication
    
    --------------------------------------------------------------------------------
    -- * An expression parser (version 1)
    
    data Expr = Num Integer
              | Add Expr Expr
              | Mul Expr Expr
              deriving (Eq,Show)
    
    eval :: Expr -> Integer
    eval (Num n) = n
    eval (Add a b) = eval a + eval b
    eval (Mul a b) = eval a * eval b
    
    {- EBNF:
    expr   ::= term {"+" term}.
    term   ::= factor {"*" factor}.
    factor ::= number | "(" expr ")".
    -}
    {-
    expr, term, factor :: Parser Expr
    
    expr = do t <- term
              ts <- zeroOrMore (do char '+'; term)
              return (foldl1 Add (t:ts))
    
    term = do f <- factor
              fs <- zeroOrMore (do char '*'; factor)
              return (foldl1 Mul (f:fs))
    
    factor = -- Num <$> number
             do n <- number
                return (Num n)
             <|>
             do char '('
                e <- expr
                char ')'
                return e
    -}
    --------------------------------------------------------------------------------
    -- * A more elegant expression parser
    
    
    expr, term, factor :: Parser Expr
    expr = leftAssoc Add term (char '+')
    term = leftAssoc Mul factor (char '*')
    factor = (Num <$> number) <|> (char '(' *> expr <* char ')')
    
    
    
    -- | Parse a list of items with separators
    -- (also available in the Parsing module)
    chain :: Parser item -> Parser sep -> Parser [item]
    chain item sep = do i <- item
                        is <- zeroOrMore (do sep; item)
                        return (i:is)
    
    leftAssoc :: (t->t->t) -> Parser t -> Parser sep -> Parser t
    leftAssoc op item sep = do is <- chain item sep
                               return (foldl1 op is)
    
    rightAssoc op item sep = undefined -- exercise
    
    --------------------------------------------------------------------------------
    -- * The simple calculator example
    
    main = do putStrLn "Welcome to the simple calculator!"
              forever readEvalPrint
    
    readEvalPrint = do putStr "Expression? "
                       s <- getLine
                       case parse expr s of
                         Just (e,"") -> do putStr "Value: "
                                           print (eval e)
                         _ -> putStrLn "Syntax error!"
    
    
    
    

   
    
    
    
    
    
    
    
    --------------------------------------------------------------------------------
    -- * More examples
    
    -- ** Data types with infix operatos
    infixl 6 :+
    infixl 7 :*
    
    data Expr2 = C Integer
               | Expr2 :+ Expr2
               | Expr2 :* Expr2
               deriving (Show,Read)  -- gives us almost what we want
    
    ex1 = C 2
    ex2 = ex1 :+ ex1
    ex3 = C 1 :+ C 2 :* C 3
    ex4 = (C 1 :+ C 2) :* C 3
    
    
    -- | Parse a specific sequence of characters
    string :: String -> Parser String
    string ""    = return ""
    string (c:s) = do c' <- char c
                      s' <- string s
                      return (c':s')