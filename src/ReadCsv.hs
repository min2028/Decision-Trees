module ReadCsv where

import Dataframe
import System.IO

-- Splits lists by some conditional based on the element
splitSep :: (a -> Bool) -> [a] -> [[a]]
splitSep _ [] = []
splitSep f xs = let (ys, zs) = break f xs in ys : splitSep f (drop 1 zs)

-- Read a file as String matrix
readCSV :: FilePath -> IO [[String]]
readCSV filename = do
  contents <- readFile filename
  return [splitSep (== ',') lines | lines <- splitSep (== '\n') contents]

-- Load Dataframe from file
readCSVAsDataframe :: FilePath -> IO (Maybe Dataframe)
readCSVAsDataframe filename = do
  contents <- readCSV filename
  return $ asDataFrame contents
