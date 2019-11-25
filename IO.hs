-- | Input/Output in Haskell
-- Examples to introduce and illustrate how I/O works in Haskell
-- Functional Programming course 2018.
-- Thomas Hallgren

{-
This started as a skeleton, the definitions were filled in
during the lecture.
-}

import Data.List(sort)

main = showTheDifference2

-- | Read two numbers x & y and show the difference  x-y
showTheDifference1 :: IO ()
showTheDifference1 = do putStrLn "Enter two numbers:"
                        x <- readLn
                        y <- readLn
                        putStrLn ("The difference is: "++show (x-y))


-- | Using 'return' to return results from IO actions
getTheDifference :: IO Integer
getTheDifference = do putStrLn "Enter two numbers:"
                      x <- readLn
                      y <- readLn
                      return (x-y)

-- | Reimplementation of showTheDifference1, using getTheDifference
showTheDifference2 :: IO ()
showTheDifference2 = do d <- getTheDifference
                        putStrLn ("The difference is: "++show d)


-- | Copy a file
copyFile :: FilePath -> FilePath -> IO ()
copyFile from to = do text <- readFile from
                      writeFile to text

-- | Sort the lines in a text file
sortFile :: FilePath -> FilePath -> IO ()
sortFile from to = do text <- readFile from
                      let sorted = sortLines text
                      writeFile to sorted

sortLines = unlines . sort . lines


doTwice :: IO a -> IO (a,a)
doTwice io = do x <- io
                y <- io
                return (x,y)

don't :: IO a -> IO ()
don't io = return ()

-- | Reimplementation of getTheDifference, using doTwice
getTheDifference2 :: IO Integer
getTheDifference2 = do putStrLn "Enter two numbers:"
                       (x,y) <- doTwice readLn
                       return (x-y)

-- | An example of combining do blocks and recursion to print some numbers
numbers :: Int -> IO ()
numbers 0 = return ()
numbers n = do print n
               numbers (n-1)
