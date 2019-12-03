module Sudoku where

import Test.QuickCheck
import Data.List
import Data.Char
import Data.Maybe

------------------------------------------------------------------------------

-- | Representation of sudoku puzzles (allows some junk)
type Cell = Maybe Int -- a single cell
type Row  = [Cell]    -- a row is a list of cells

data Sudoku = Sudoku [Row] 
 deriving ( Show, Eq )

rows :: Sudoku -> [Row]
rows (Sudoku ms) = ms

-- * A1

-- | allBlankSudoku is a sudoku with just blanks
allBlankSudoku :: Sudoku
allBlankSudoku = Sudoku $ replicate 9 $ replicate 9 Nothing

-- * A2

-- | isSudoku sud checks if sud is really a valid representation of a sudoku
-- puzzle
isSudoku :: Sudoku -> Bool
isSudoku (Sudoku sud)
  | length sud /= 9       = False
  | any invalidLength sud = False
  | any invalidInt sud    = False
  | otherwise             = True
  where invalidInt r      = any (\i -> i < Just 1 || i > Just 10) $ filter (/= Nothing) r
        invalidLength r   = length r /= 9 

-- * A3

-- | isFilled sud checks if sud is completely filled in,
-- i.e. there are no blanks
isFilled :: Sudoku -> Bool
isFilled (Sudoku sud) = all (\r -> (all (/= Nothing) r)) sud

------------------------------------------------------------------------------

-- * B1

-- | printSudoku sud prints a nice representation of the sudoku sud on
-- the screen
printSudoku :: Sudoku -> IO ()
printSudoku (Sudoku rs) = mapM_ putStrLn $ [rowAsString r | r <- rs]

rowAsString :: Row -> String
rowAsString row = map (\x -> if (x == Nothing) then '.' else intToDigit $ fromJust x) row

-- * B2

-- | readSudoku file reads from the file, and either delivers it, or stops
-- if the file did not contain a sudoku
--readSudoku :: FilePath -> IO ()
readSudoku :: FilePath -> IO Sudoku
readSudoku fPath = do str <- readFile fPath
                      let sud = Sudoku $ map stringToRow (lines str)
                      if (isSudoku sud) then return sud else error "Not a valid sudoko"

stringToRow :: String -> Row
stringToRow str = map (\x -> if (x == '.') then Nothing else Just (digitToInt x)) str

-- * C1

-- | cell generates an arbitrary cell in a Sudoku
cell :: Gen Cell
cell = frequency [(1, elements [Just n | n <- [1..9]]), (2, elements [Nothing])]

-- * C2

-- | an instance for generating Arbitrary Sudokus

instance Arbitrary Sudoku where
  arbitrary = Sudoku <$> (vectorOf 9 $ vectorOf 9 cell)

 -- hint: get to know the QuickCheck function vectorOf
 
-- * C3

prop_Sudoku :: Sudoku -> Bool
prop_Sudoku sud = isSudoku sud 
  
------------------------------------------------------------------------------

type Block = [Cell] -- a Row is also a Cell

-- * D1

isOkayBlock :: Block -> Bool
isOkayBlock b = length numList == length (nub numList)
  where numList = filter (/= Nothing) b

-- * D2

blocks :: Sudoku -> [Block]
blocks (Sudoku rows) = rows ++ columnBlocks rows ++ squareBlocks rows

columnBlocks :: [Row] -> [Block]
columnBlocks rs = [map (!!b) rs | b <- [0..8]]

squareBlocks :: [Row] -> [Block]
squareBlocks rs = [squareBlock rs (r, c) | r <- [0..2], c <- [0..2]]

squareBlock :: [Row] -> (Int, Int) -> Block
squareBlock allRows (ri, ci) = [takeCell allRows (r, c) | (r, c) <- cellIndexes]
  where cellIndexes = [(r, c) | r <- take 3 [ri*3 ..], c <- take 3 [ci*3 ..]]

takeCell :: [Row] -> (Int, Int) -> Cell
takeCell rs (r, c) = (rs!!r)!!c

prop_blocks_lengths :: Sudoku -> Bool
prop_blocks_lengths sud = length allBlocks == 27 && all (\b -> length b == 9) allBlocks
  where allBlocks = blocks sud

-- * D3

isOkay :: Sudoku -> Bool
isOkay sud = all isOkayBlock $ blocks sud
      

---- Part A ends here --------------------------------------------------------
------------------------------------------------------------------------------
---- Part B starts here ------------------------------------------------------

-- Test data
example :: Sudoku
example =
  Sudoku
    [[j 3,j 6,n  ,n  ,j 7,j 1,j 2,n  ,n  ],
     [n  ,j 5,n  ,n  ,n  ,n  ,j 1,j 8,n  ],
     [n  ,n  ,j 9,j 2,n  ,j 4,j 7,n  ,n  ],
     [n  ,n  ,n  ,n  ,j 1,j 3,n  ,j 2,j 8],
     [j 4,n  ,n  ,j 5,n  ,j 2,n  ,n  ,j 9],
     [j 2,j 7,n  ,j 4,j 6,n  ,n  ,n  ,n  ],
     [n  ,n  ,j 5,j 3,n  ,j 8,j 9,n  ,n  ],
     [n  ,j 8,j 3,n  ,n  ,n  ,n  ,j 6,n  ],
     [n  ,n  ,j 7,j 6,j 9,n  ,n  ,j 4,j 3]]
  where
    n = Nothing
    j = Just

-- | Positions are pairs (row,column),
-- (0,0) is top left corner, (8,8) is bottom left corner
type Pos = (Int,Int)

-- * E1

blanks :: Sudoku -> [Pos]
blanks (Sudoku rs) = [(r, c) | r <- [0..8], c <- [0..8], takeCell rs (r, c) == Nothing]

prop_blanks_allBlanks :: Sudoku -> Bool
prop_blanks_allBlanks sud = all (\(r, c) -> takeCell rs (r, c) == Nothing) $ blanks sud
    where rs = rows sud

-- * E2

(!!=) :: [a] -> (Int,a) -> [a]
(!!=) xs (i,y)
  | i < 0           = error "Index to insert can not be negative"
  | length xs == 0  = []
  | length xs <= i  = error "Index to insert is larger than list"
  | otherwise       = as ++ [y] ++ bs 
    where (as, _: bs) = splitAt i xs 

prop_bangBangEquals_correct :: [Maybe Int] -> (Int, Maybe Int) -> Bool
prop_bangBangEquals_correct xs (i, x) 
  | length xs == 0 = inserted == []
  | x == deleted   = inserted == xs && sameSize
  | otherwise      = inserted !!= (index, deleted) == xs && sameSize
    where index    = if (length xs == 0) then 0 else i `mod` length xs 
          deleted  = xs !! index
          inserted = xs !!= (index, x)
          sameSize = length inserted == length xs

-- * E3

update :: Sudoku -> Pos -> Cell -> Sudoku
update (Sudoku rs) (r, c) newC = Sudoku $ rs !!= (r, updatedRow)
    where updatedRow = (rs!!r) !!= (c, newC)

prop_update_updated :: Sudoku -> Pos -> Cell -> Bool
prop_update_updated sud (r, c) newC = takeCell updated pos == newC
    where updated = rows $ update sud pos newC
          pos     = (if (abs r > 8) then abs r `mod` 8 else abs r, if (abs c > 8) then abs c `mod` 8 else abs c) 

------------------------------------------------------------------------------

-- * F1

solve :: Sudoku -> Maybe Sudoku
solve sud = if (length s == 0) then Nothing else Just $ head s
  where s = take 1 $ solve' sud (blanks sud)

solve' :: Sudoku -> blankCells -> [Sudoku]
solve' sud bs
  | not(isSudoku sud) || not(isOkay sud) = []
  | isFilled sud = [sud]
  | otherwise = filter (\s -> isOkay s && isSudoku s && isFilled s) 
                $ concat [solve' (update sud (head $ blanks sud) $ Just num) (drop 1 $ blanks sud) | num <- [0..9]]   

-- * F2

readAndSolve :: FilePath -> IO ()
readAndSolve fp = do sud <- readSudoku fp
                     let solved = solve sud
                     if (solved == Nothing) then putStrLn "(no solutions)" else printSudoku (fromJust solved)

-- * F3

isSolutionOf :: Sudoku -> Sudoku -> Bool
isSolutionOf sud1 sud2 = isOkay sud1 && isFilled sud1 
                         && and [c1 == c2 | (c1, c2) <- zip (concat $ rows sud1) (concat $ rows sud2), c2 /= Nothing]               

-- * F4
prop_SolveSound :: Sudoku -> Bool
prop_SolveSound sud 
  | not(isOkay sud) || not(isSudoku sud) = solve sud == Nothing
  | otherwise = and [sul `isSolutionOf` sud | sul <- solve' sud (blanks sud)]