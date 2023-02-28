module Dataframe
  ( FValue,
    ColumnType,
    Dataframe (..),
    dfExample,
    getColumn,
    getColumnIndex,
    getLength,
    asDataFrame,
    getColumnUniqueValues,
  )
where

import Data.List
import Text.Read

-- Define the FValue type
data FValue
  = FString String
  | FNumber Double
  | FMissing
  deriving (Show, Eq, Ord)

-- Define ColumnTypes
data ColumnType = Nominal | Quantitative deriving (Show)

-- Define a dataframe type
data Dataframe = Dataframe
  { headers :: [String],
    columnTypes :: [ColumnType],
    rows :: [[FValue]]
  }
  deriving (Show)

-- Example df
dfExample = (Dataframe {headers = ["colour", "edible"], columnTypes = [Nominal, Nominal], rows = [[FString "red", FString "yes"], [FString "red", FString "yes"], [FString "green", FString "no"], [FString "green", FString "no"], [FString "red", FString "no"]]})

-- Returns a list of values corresponding to the column index
getColumn :: Dataframe -> Int -> [FValue]
getColumn df index = map (!! index) (rows df)

-- Get the index of a column in a dataframe
getColumnIndex :: [String] -> String -> Int
getColumnIndex hs col = head [i | (i, name) <- zip [0 ..] hs, name == col]

-- Returns a list of the unique values corresponding to the column index
getColumnUniqueValues :: Dataframe -> Int -> [FValue]
getColumnUniqueValues df index = nub (getColumn df index)

-- Returns the length of the dataframe
getLength :: Dataframe -> Int
getLength df = length (rows df)

-- Parse loaded data into a dataframe
asDataFrame :: [[String]] -> Maybe Dataframe
asDataFrame [] = Nothing
asDataFrame (headers : rows) = do
  let colTypes = getColumnTypes rows
  let parsedRows = parseRows colTypes rows
  return $ Dataframe headers colTypes parsedRows

-- Parses each String in a column to check if it is a number or string
getColumnTypes :: [[String]] -> [ColumnType]
getColumnTypes rows =
  map (\col -> if all isNumber col then Quantitative else Nominal) (transpose rows)

-- Parses each row based on the column type
parseRows :: [ColumnType] -> [[String]] -> [[FValue]]
parseRows colTypes rows =
  let parseValue (Nominal, s) = FString s
      parseValue (Quantitative, s) = maybe FMissing FNumber (readMaybe s :: Maybe Double)
      parseRow row = map parseValue (zip colTypes row)
   in map parseRow rows

-- Checks if a String is a number
isNumber :: String -> Bool
isNumber = all (`elem` "-.0123456789")
