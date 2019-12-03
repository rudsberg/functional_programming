import Data.List
sortFile :: FilePath -> IO ()

sortFile filename =
  do input <- readFile filename
     l <- lines input
     sortedString <- sort l
     writeFile filename sortedString