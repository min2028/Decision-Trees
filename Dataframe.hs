module Dataframe where

import Data.List
import Text.Read

-- Define the FValue type
data FValue
  = FString String
  | FNumber Double
  | FMissing
  deriving (Show, Eq, Ord)

-- Define ColumnTypes
data ColumnType = String | Number deriving (Show)

-- Define a dataframe type
data Dataframe = Dataframe
  { headers :: [String],
    columnTypes :: [ColumnType],
    rows :: [[FValue]]
  }
  deriving (Show)

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
  map (\col -> if all isNumber col then Dataframe.Number else Dataframe.String) (transpose rows)

-- Parses each row based on the column type
parseRows :: [ColumnType] -> [[String]] -> [[FValue]]
parseRows colTypes rows =
  let parseValue (Dataframe.String, s) = FString s
      parseValue (Dataframe.Number, s) = maybe FMissing FNumber (readMaybe s :: Maybe Double)
      parseRow row = map parseValue (zip colTypes row)
   in map parseRow rows

-- Checks if a String is a number
isNumber :: String -> Bool
isNumber = all (`elem` "-.0123456789")
