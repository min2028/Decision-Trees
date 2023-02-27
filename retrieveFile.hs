module RetrieveFile where

import Data.List
import Data.Maybe
import System.IO
import Text.Read

-- Define the FValue type
data FValue
  = QuantitativeF (Either Int Double)
  | NominalF [Char]
  | MissingF
  deriving (Show, Eq, Ord)

-- Define a dataframe type
data Dataframe = Dataframe
  { headers :: [String],
    columnDatatypes :: [String],
    values :: [[FValue]]
  }
  deriving (Show)

-- Splits lists by some conditional based on the element
splitsep :: (a -> Bool) -> [a] -> [[a]]
splitsep _ [] = []
splitsep f xs = let (ys, zs) = break f xs in ys : splitsep f (drop 1 zs)

-- Read a file as String matrix
readCSV :: FilePath -> IO [[String]]
readCSV filename = do
  contents <- readFile filename
  return [splitsep (== ',') lines | lines <- splitsep (== '\n') contents]

-- Load Dataframe from file
readFileAsDataframe :: FilePath -> IO Dataframe
readFileAsDataframe filename = do
  contents <- readCSV filename
  return $ asDataFrame contents

-- Parse loaded data into a dataframe
asDataFrame :: [[String]] -> Dataframe
asDataFrame (headers : rows) =
  let numColumns = length headers
      columnDatatypes = map getListFValueAsString columnData
      columnData = map (map asFValue) $ transpose rows
   in Dataframe
        { headers = headers,
          columnDatatypes = columnDatatypes,
          values = columnData
        }
asDataFrame _ =
  Dataframe
    { headers = [],
      columnDatatypes = [show MissingF],
      values = []
    }

-- Parse a String to an FValue
asFValue :: String -> FValue
asFValue str
  | isJust maybeInt = QuantitativeF $ Left (fromJust maybeInt)
  | isJust maybeDouble = QuantitativeF $ Right (fromJust maybeDouble)
  | str == "" = MissingF
  | otherwise = NominalF str
  where
    maybeInt = readMaybe str :: Maybe Int
    maybeDouble = readMaybe str :: Maybe Double

-- Parse a list of FValues' type to String
getListFValueAsString :: [FValue] -> String
getListFValueAsString x =
  case nub (map getFValueAsString x) of
    [x] -> x
    _ -> "Mixed"

-- Get FValue's type as String
getFValueAsString :: FValue -> String
getFValueAsString fValue =
  case fValue of
    QuantitativeF _ -> "QuantitativeF"
    NominalF _ -> "NominalF"
    MissingF -> "MissingF"
