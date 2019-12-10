import Data.List

{-
sortFile :: FilePath -> IO ()

sortFile filename =
  do input <- readFile filename
     l <- lines input
     sortedString <- sort l
     writeFile filename sortedString
-}


data Expr = Val Int | Div Expr Expr

eval :: Expr -> Maybe Int
eval (Val i)   = Just i
eval (Div x y) = cases (eval x) (\n -> cases (eval y) (\m -> safeDiv n m))

eval' :: Expr -> Maybe Int
eval' (Val i)   = Just i
eval' (Div x y) = do
                    n <- eval x
                    m <- eval y
                    n `safeDiv` m
                    
-- | Cases used as binding operator >>= 
cases :: Maybe Int -> (Int -> Maybe Int) -> Maybe Int
cases m f = case m of
                Nothing -> Nothing
                Just x -> f x 

safeDiv :: Int -> Int -> Maybe Int
safeDiv m n = if n == 0 then Nothing else Just $ div m n 
