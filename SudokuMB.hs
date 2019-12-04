module Sudoku where
  import Test.QuickCheck
  import Data.Maybe
  import Data.Char
  import Data.List

  ------------------------------------------------------------------------------
  -- | Representation of sudoku puzzles (allows some junk)
  type Cell = Maybe Int -- a single cell
  type Row  = [Cell]    -- a row is a list of cells
  data Sudoku = Sudoku [Row]  -- [[[Maybe Int]]]
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
 
-- | Harder example with more empty spaces
  example3 :: Sudoku
  example3 = Sudoku
                [ [n  ,j 6,n  ,n  ,n  ,j 1,n  ,n  ,n  ]
                , [n  ,j 5,n  ,n  ,n  ,n  ,j 1,n  ,n  ]
                , [n  ,n  ,j 9,j 2,n  ,j 4,n  ,n  ,n  ]
                , [n  ,n  ,n  ,n  ,j 1,n  ,n  ,j 2,j 8]
                , [j 4,n  ,n  ,n ,n  ,j 2,n  ,n  ,j 9]
                , [j 2,j 7,n  ,n  ,j 6,n  ,n  ,n  ,n  ]
                , [n  ,n  ,n  ,n  ,n  ,j 8,n  ,n  ,n  ]
                , [n  ,n  ,n  ,n  ,n  ,n  ,n  ,j 6,n  ]
                , [n  ,n  ,j 7,n  ,j 9,n  ,n  ,n  ,j 3]
                ]
            where
              n = Nothing
              j = Just
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
  printSudoku (Sudoku rs) = putStr $ printRow $ rs

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
  cell = frequency [(4, elements [Just n | n <- [1..9]]),(5,elements [Nothing])]

  -- * C2
  -- | an instance for generating Arbitrary Sudokus
  instance Arbitrary Sudoku where
    arbitrary = Sudoku <$> (vectorOf 9 $ vectorOf 9 cell)
  
  -- * C3
  prop_Sudoku :: Sudoku -> Bool
  prop_Sudoku sud = isSudoku sud

  ------------------------------------------------------------------------------
  type Block = [Cell] -- a Row is also a Cell
  -- * D1
  -- | Check if there is any digits that appears more than once
  isOkayBlock :: Block -> Bool
  isOkayBlock block = length (nub blockDigits) == length blockDigits
      where blockDigits = filter (/= Nothing) block

  -- * D2
  blocks :: Sudoku -> [Block]
  blocks (Sudoku rows) = rows ++ cols rows ++ squareBlocks rows        

  -- | Creates a block with columns from a list of rows 
  cols :: [Row] -> [Block]
  cols rows = [map (!! i) rows | i <- [0..8]]

  -- | Given a list of rows and a position, return cell value of that position
  takeCell :: [Row] -> (Int, Int) -> Cell
  takeCell rs (r, c) = (rs!!r)!!c

  squareBlocks :: [Row] -> [Block]
  squareBlocks rows = [squareBlock (c*3,0) rows | c <- [0..2]] ++ 
                      [squareBlock (c*3,3) rows | c <- [0..2]] ++ 
                      [squareBlock (c*3,6) rows | c <- [0..2]]


  -- | Creates a 3x3 block as a list of cells from a list of rows and given indices
  squareBlock :: (Int,Int) -> [Row] -> [Cell]
  squareBlock (colInd,rowInd) rows = concat [map (!! i) columns | i <- [rowInd..rowInd+2]] 
            where columns  = [map (!! i) rows | i <- [colInd..colInd+2]]

  -- | Test that given sudoku contains  
  prop_blocks_lengths :: Sudoku -> Bool
  prop_blocks_lengths (Sudoku sud) = and (map (\x -> length x == 9) bs) && length bs == 27
    where bs = blocks (Sudoku sud)

  -- * D3
  isOkay :: Sudoku -> Bool
  isOkay (Sudoku s) = all (isOkayBlock) $ blocks (Sudoku s) 


  ---- Part A ends here --------------------------------------------------------
  ------------------------------------------------------------------------------


  ---- Part B starts here ------------------------------------------------------

  -- | Positions are pairs (row,column),
  -- (0,0) is top left corner, (8,8) is bottom left corner
  type Pos = (Int,Int)

  -- * E1 Given a Sudoku returns a list of the positions in the Sudoku that are still blank.
  blanks :: Sudoku -> [Pos]
  blanks (Sudoku sud) =  blankRow sud 0

  
  blankRow :: [Row] -> Int -> [Pos]
  blankRow [] _     = []
  blankRow (r:rs) i = zip iList blank ++ blankRow rs (i + 1)
      where blank = findIndices (== Nothing) r 
            iList = replicate (length blank) i  


  -- | Check that all elements in the list of blanks actually is blank
  --   by filtering out all 'Nothing' elements and see if something is left
  prop_blanks_allBlanks :: Sudoku -> Bool
  prop_blanks_allBlanks sud = length (filter (\(r,c) -> takeCell rs (r,c) /= Nothing) $ blanks sud) == 0
      where rs = rows sud

  -- * E2
  -- | Given a list and a tuple containing an index in the list and a new value, updates the given list with the new value at 
  (!!=) :: [a] -> (Int,a) -> [a]
  (!!=) list (i,y) | length list < 0  = error "Can not change value in an empty list"
                   | length list == 0 = []
                   | i >= length list = error "Index bigger than list" 
                   | otherwise        = head ++ [y] ++ tail
       where (head, _:tail) = splitAt i list 


  -- | Checks that !!= operator work as intended
  prop_bangBangEquals_correct :: [Maybe Int] -> (Int,(Maybe Int)) -> Bool
  prop_bangBangEquals_correct list (i,v) | length list == 0  = (list !!= (0,v)) == []
                                         | list !! i' == v   = list == list !!= (i',v) 
                                         | otherwise         = list /= list !!= (i',v) 
      where i' = if abs i >= length list then mod i $ length list else abs i

  -- * E3
  -- | Updates a given sudoku with a given value at a given position
  update :: Sudoku -> Pos -> Cell -> Sudoku
  update (Sudoku sud) (r,c) val = Sudoku (head ++ [upDatedRow] ++ tail)
      where upDatedRow = (sud !! r) !!= (c,val)  
            (head, _:tail) = splitAt r sud 

  -- | Checks that function update work as intended
  prop_update_updated :: Sudoku -> Pos -> Cell -> Bool
  prop_update_updated s (i,j) c | c == takeCell (rows s) p' = s == update s p' c 
                                | otherwise                 = s /= update s p' c
              where p' = if abs i > 8 || abs j > 8 then (mod i 8, mod j 8) else (abs i, abs j) -- Could maybe have restricted (i,j) with a property test instead
  ------------------------------------------------------------------------------
  -- * F1
  -- | Produces one solution (if there is one) for a given sudoku
  solve :: Sudoku -> Maybe Sudoku
  solve sudoku = if length solution == 0 then Nothing else Just $ head solution
    where solution = take 1 $ solve' sudoku $ blanks sudoku


  solve' :: Sudoku -> [Pos] -> [Sudoku]
  solve' sudoku blank | (not $ isSudoku sudoku) || (not $ isOkay sudoku) = []
                      | isFilled sudoku && isOkay sudoku                 = [sudoku] 
                      | otherwise                                        = filter (\x -> isOkay x && isFilled x) solution
       where solution = concat [solve' (update sudoku (head blank) (Just c)) (drop 1 blank) | c <- [1..9]]
                      
  -- * F2 
  -- | Reads a sudoku from a file, solves it and prints the solution if found
  readAndSolve :: FilePath -> IO ()
  readAndSolve file = do
                      sud <- readSudoku file 
                      let solved = solve sud
                      if solved == Nothing then putStr "No solution found\n" else printSudoku (fromJust $ solve sud)
                  
  -- * F3 
  -- | Checks whether the first sudoku is a solution to the second sudoku
  isSolutionOf :: Sudoku -> Sudoku -> Bool
  isSolutionOf s1 s2 = isSudoku s1 && isOkay s1 && isFilled s1 && containsAll
      where containsAll = all (\(x1,x2) -> x1 == x2) $ filter(\(c1,c2) -> c2 /= Nothing) $ zip (concat $ rows s1) (concat $ rows s2)

  -- * F4 
  -- | Tests if the solution to a sudoku is a valid solution
  prop_SolveSound :: Sudoku -> Property
  prop_SolveSound sudoku = isSudoku sudoku && isOkay sudoku ==> solveSound (solve sudoku) sudoku
      where solveSound :: Maybe Sudoku -> Sudoku -> Bool
            solveSound (Just sudoku) sud = isSolutionOf (fromJust $ Just sudoku) sud
            solveSound Nothing _ = False

  fewerChecks prop = quickCheckWith stdArgs{maxSuccess=30 } prop