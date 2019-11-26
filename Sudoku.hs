module Sudoku where
  import Test.QuickCheck
  import Data.Maybe
  import Data.Char

  ------------------------------------------------------------------------------
  -- | Representation of sudoku puzzles (allows some junk)
  type Cell = Maybe Int -- a single cell
  type Row  = [Cell]    -- a row is a list of cells
  data Sudoku = Sudoku [Row] 
   deriving ( Show, Eq )
  rows :: Sudoku -> [Row]
  rows (Sudoku ms) = ms
  -- | A sample sudoku puzzle
  example :: Sudoku
  example =
      Sudoku
        [ [j 3,j 6,n  ,n  ,j 7,j 1,j 2,n  ,n  ]
        , [n  ,j 5,n  ,n  ,n  ,n  ,j 1,j 8,n  ]
        , [n  ,n  ,j 9,j 2,n  ,j 4,j 7,n  ,n  ]
        , [n  ,n  ,n  ,n  ,j 1,j 3,n  ,j 2,j 8]
        , [j 4,n  ,n  ,j 5,n  ,j 2,n  ,n  ,j 9]
        , [j 2,j 7,n  ,j 4,j 6,n  ,n  ,n  ,n  ]
        , [n  ,n  ,j 5,j 3,n  ,j 8,j 9,n  ,n  ]
        , [n  ,j 8,j 3,n  ,n  ,n  ,n  ,j 6,n  ]
        , [n  ,n  ,j 7,j 6,j 9,n  ,n  ,j 4,j 3]
        ]
    where
      n = Nothing
      j = Just
  -- | test example on filled sudoku
  example2 :: Sudoku
  example2 =
          Sudoku
            [ [j 3,j 6, j 1 ,j 2 ,j 7,j 1,j 2, j 3  ,j 4  ]
            , [j 6  ,j 5,j 7  ,j 9  ,j 1  ,j 3  ,j 1,j 8,j 5  ]
            , [j 6  ,j 7  ,j 9,j 2,j 2  ,j 4,j 7,j 5  ,j 5  ]
            , [j 6  ,j 7  ,j 9,j 2,j 2  ,j 4,j 7,j 5  ,j 5  ]
            , [j 6  ,j 7  ,j 9,j 2,j 2  ,j 4,j 7,j 5  ,j 5  ]
            , [j 6  ,j 7  ,j 9,j 2,j 2  ,j 4,j 7,j 5  ,j 5  ]
            , [j 6  ,j 7  ,j 9,j 2,j 2  ,j 4,j 7,j 5  ,j 5  ]
            , [j 6  ,j 7  ,j 9,j 2,j 2  ,j 4,j 7,j 5  ,j 5  ]
            , [j 6  ,j 7  ,j 9,j 2,j 2  ,j 4,j 7,j 5  ,j 5  ]
            ]
        where
          n = Nothing
          j = Just
  
  {-
  type Cell   = Maybe Int
  type Row    = [Cell]
  data Sudoku = Sudoku [Row]
  -}
  
  -- * A1
  -- | allBlankSudoku is a sudoku with just blanks
  allBlankSudoku :: Sudoku
  allBlankSudoku =  Sudoku (replicate 9 (replicate 9 Nothing))
  
  -- * A2
  -- | isSudoku sud checks if sud is really a valid representation of a sudoku
  -- puzzle
  isSudoku :: Sudoku -> Bool
  isSudoku (Sudoku rs) | rows sudoku == [] = False
                       | otherwise         = (isSudSize $ rows sudoku) && (validRows (rows sudoku) ([Just n | n <- [1..9]] ++ [Nothing]))
              where sudoku = (Sudoku rs)
  
  
  -- | Returns true if sudoku size is a 9 x 9 matrix
  isSudSize :: [Row] -> Bool
  isSudSize (r:rs) | length r == 0  = False
                   | otherwise      = (and $ map (== 9) rowSizes) && (length rowSizes == 9)
              where rowSizes = (length r):rowSize rs
   
  -- | Given a list of rows, calc length of row and return a list containing length of every row
  rowSize :: [Row] -> [Int]
  rowSize [] = []
  rowSize (row:rows) = (length row):lenList
      where lenList = rowSize rows
     
  -- | Given a list of rows, check if content is valid (cells in row matching second argument with valid cellvalues )
  validRows :: [Row] -> [Cell] -> Bool
  validRows [] _ = True
  validRows (row:rows) validValues  = validCells row validValues && validRows rows validValues
  
  -- | Given two list of cells, check that each cell matching the list with valid cell values
  validCells :: [Cell] -> [Cell] -> Bool
  validCells [] _ = True
  validCells (c:cs) validValues = c `elem` validValues && validCells cs validValues
   
  
  -- * A3
  -- | isFilled sud checks if sud is completely filled in,
  -- i.e. there are no blanks
  isFilled :: Sudoku -> Bool
  isFilled (Sudoku rs) = validRows (rows sudoku) [Just n | n <- [1..9]]
      where sudoku = (Sudoku rs)
  
  ------------------------------------------------------------------------------
  -- * B1
  -- | printSudoku sud prints a nice representation of the sudoku sud on
  -- the screen
  printSudoku :: Sudoku -> IO ()
  printSudoku (Sudoku rs) = putStr $ printRow $ rows (Sudoku rs)

  -- | Creates a String representation of each row in the sudoku
  printRow :: [Row] -> String
  printRow []     = ""
  printRow (r:rs) = printCell r ++ '\n':printRow rs
    where printCell :: [Cell] -> String
          printCell []  = ""
          printCell row = map (\x -> if (x == Nothing) then '.' else intToDigit (fromJust x)) row
    
  -- * B2
  -- | readSudoku file reads from the file, and either delivers it, or stops
  -- if the file did not contain a sudoku

  readSudoku :: FilePath -> IO Sudoku
  readSudoku file = do sudStr <- readFile file
                       let sud = map stringToRow (lines sudStr)
                       if isSudoku (Sudoku sud) then return (Sudoku sud) else error "Error, not a sudoku!"
                  

  stringToRow :: String -> Row
  stringToRow str =  map (\c -> if c == '.' then Nothing else Just (digitToInt c)) str
                   
  ------------------------------------------------------------------------------
  -- * C1
  -- | cell generates an arbitrary cell in a Sudoku
  cell :: Gen (Cell)
  cell = frequency [(3, elements [Just n | n <- [1..9]]),(7,elements [Nothing])]

  -- * C2
  -- | an instance for generating Arbitrary Sudokus
  instance Arbitrary Sudoku where
    arbitrary = Sudoku (vectorOf 9 (vectorOf 9 cell))
  



   -- hint: get to know the QuickCheck function vectorOf
  -- * C3
  prop_Sudoku :: Sudoku -> Bool
  prop_Sudoku = undefined
    -- hint: this definition is simple!
  ------------------------------------------------------------------------------
  type Block = [Cell] -- a Row is also a Cell
  -- * D1
  isOkayBlock :: Block -> Bool
  isOkayBlock = undefined
  -- * D2
  blocks :: Sudoku -> [Block]
  blocks = undefined
  prop_blocks_lengths :: Sudoku -> Bool
  prop_blocks_lengths = undefined
  -- * D3
  isOkay :: Sudoku -> Bool
  isOkay = undefined
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
  
  
  