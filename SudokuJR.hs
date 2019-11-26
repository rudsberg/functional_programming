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
printSudoku (Sudoku sud) = mapM_ putStrLn $ [rowAsString r | r <- sud]

rowAsString :: Row -> String
rowAsString row = map (\x -> if (x == Nothing) then '.' else intToDigit $ fromJust x) row

-- * B2

-- | readSudoku file reads from the file, and either delivers it, or stops
-- if the file did not contain a sudoku
--readSudoku :: FilePath -> IO ()
readSudoku :: FilePath -> IO Sudoku
readSudoku fPath = do str <- readFile fPath
                      let sud = Sudoku $ map convertStringToRow (lines str)
                      if (isSudoku sud) then return sud else error "Not a valid sudoko"

convertStringToRow :: String -> Row
convertStringToRow str = map (\x -> if (x == '.') then Nothing else Just (digitToInt x)) str

-- * C1

-- | cell generates an arbitrary cell in a Sudoku
cell :: Gen Cell
cell = frequency [(1, elements [Just n | n <- [1..9]]), (2, elements [Nothing])]

-- * C2

-- | an instance for generating Arbitrary Sudokus

instance Arbitrary Sudoku where
  arbitrary = Sudoku <$> (vectorOf 9 (vectorOf 9 cell)) -- HOW DOES THIS WORK WITH FMAP?!


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
blocks (Sudoku rows) = rowBlocks ++ columnBlocks ++ squareBlocks
  where rowBlocks, columnBlocks, squareBlocks :: [Block]
        rowBlocks = rows
        columnBlocks = [map (!!b) rows | b <- [0..8]]
        squareBlocks = [squareBlock rows (r, c) | r <- [0..2], c <- [0..2]]


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
isOkay (Sudoku rows) = okRows && okColumns && okBlocks
  where okRows, okColumns, okBlocks :: Bool
        okRows = all isOkayBlock rows   
        okColumns = all isOkayBlock $ [map (!!b) rows | b <- [0..8]]
        okBlocks = all isOkayBlock [squareBlock rows (r, c) | r <- [0..2], c <- [0..2]]

        

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

exRow :: Row
exRow = [Just 1, Just 3, Nothing, Just 8]
filled :: Sudoku
filled = Sudoku 
  [ [Just 3, Just 1, Just 3]
  , [Just 4, Just 9, Just 3]
  , [Just 3, Just 1, Just 3]
  ]
notFilled = Sudoku 
  [ [Just 3, Just 1, Just 3]
  , [Just 4, Just 9, Nothing]
  , [Just 3, Just 1, Just 3]
  ]


---- Part A ends here --------------------------------------------------------
------------------------------------------------------------------------------
---- Part B starts here ------------------------------------------------------


-- | Positions are pairs (row,column),
-- (0,0) is top left corner, (8,8) is bottom left corner
type Pos = (Int,Int)

-- * E1

blanks :: Sudoku -> [Pos]
blanks = undefined

--prop_blanks_allBlanks :: ...
--prop_blanks_allBlanks =


-- * E2

(!!=) :: [a] -> (Int,a) -> [a]
xs !!= (i,y) = undefined

--prop_bangBangEquals_correct :: ...
--prop_bangBangEquals_correct =


-- * E3

update :: Sudoku -> Pos -> Cell -> Sudoku
update = undefined

--prop_update_updated :: ...
--prop_update_updated =


------------------------------------------------------------------------------

-- * F1


-- * F2


-- * F3


-- * F4
