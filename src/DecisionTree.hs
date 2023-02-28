module DecisionTree where

import Data.List
import qualified Data.Map as Map
import Data.Ord
import Dataframe

data DTtree a b
  = DTNode ((a, b) -> Bool) (DTtree a b) (DTtree a b)
  | LeafNode b

instance (Show a, Show b) => Show (DTtree a b) where
  show (LeafNode b) = show b
  show (DTNode p lt rt) = "<" ++ showFunction p ++ ": " ++ show lt ++ " | " ++ show rt ++ ">"

showFunction :: ((a, b) -> Bool) -> String
showFunction p = "function"

isThisLeafPure :: Eq b => [(a, b)] -> Bool
isThisLeafPure [] = True
isThisLeafPure [(a, b)] = True
isThisLeafPure ((a, b) : t) = foldr (\(x, y) z -> (y == b) && z) True ((a, b) : t)

-- Tests
-- isThisLeafPure []
-- isThisLeafPure [(30, 40)]
-- isThisLeafPure [(30, 40), (40, 40)]
-- isThisLeafPure [(30, 40), (40, 50)]
-- isThisLeafPure [(30, "h"), (40, "h")]
-- isThisLeafPure [(30, "y"), (40, "n")]
-- isThisLeafPure [("h", "yes"), ("i", "yes"), ("j", "yes")]

-- Splits a dataframe into two dataframes based on the column, String, and value, FValue
-- (dataframe, columnIndex, value)
splitDataframe :: Dataframe -> Int -> FValue -> (Dataframe, Dataframe)
splitDataframe (Dataframe hs cs rs) colIndex val = (Dataframe hs cs leftRows, Dataframe hs cs rightRows)
  where
    leftRows = filter (\row -> row !! colIndex == val) rs
    rightRows = filter (\row -> row !! colIndex /= val) rs

-- splitDataframe dfExample "colour" $ FString "green"
-- (Dataframe {headers = ["colour","edible"], columnTypes = [Nominal,Nominal], rows = [[FString "green",FString "no"],[FString "green",FString "no"]]},Dataframe {headers = ["colour","edible"], columnTypes = [Nominal,Nominal], rows = [[FString "red",FString "yes"],[FString "red",FString "yes"],[FString "red",FString "no"]]})

-- Get the best column to split on
-- (dataframe, targetColumn) -> splitColumn
{-nextFeatureToSplit :: Dataframe -> String -> String
nextFeatureToSplit df targetColumn =
  let targetIndex = getColumnIndex (headers df) targetColumn
      numFeatures = length (headers df) - 1
      splitScores = map (\i -> (headers df !! i, computeGiniIndex df targetIndex (splitDataframe df ))) [0 .. numFeatures]
      bestSplit = maximumBy (comparing snd) splitScores
   in fst bestSplit
-}

-- Computes the GINI index for a given column
-- (featureIndex, valueSpltOn, (yesDataframe, noDataframe)) -> giniIndex
computeGiniIndex :: (Int, FValue, (Dataframe, Dataframe)) -> Double
computeGiniIndex (columnIndex, _, (yesDf, noDf)) =
  let totalCount = yesCount + noCount
      yesCount = fromIntegral $ getLength yesDf
      noCount = fromIntegral $ getLength noDf
      yesFraction = yesCount / totalCount
      noFraction = noCount / totalCount
      yesGini = computeFractionGini yesDf columnIndex
      noGini = computeFractionGini noDf columnIndex
      gini = (yesFraction * yesGini) + (noFraction * noGini)
   in gini

-- Gets all possible splits for each column
-- (dataframe, targetIndex, featureIndex) -> [(featureIndex, valueSpltOn, (yesDataframe, noDataframe))]
getPossibleSplits :: Dataframe -> Int -> Int -> [(Int, FValue, (Dataframe, Dataframe))]
getPossibleSplits df targetIndex featureIndex = [(featureIndex, value, splitDataframe df featureIndex value) | value <- getColumnUniqueValues df featureIndex]

-- Computes the gini score for a given dataframe of either yes or no
-- (dataframe, targetIndex) -> fracGiniIndex
computeFractionGini :: Dataframe -> Int -> Double
computeFractionGini df targetIndex =
  let totalRows = length (rows df)
      targetCounts = countTargetValues df targetIndex
      targetProbabilities = map (\count -> fromIntegral count / fromIntegral totalRows) targetCounts
   in 1.0 - sum (map (\p -> p * p) targetProbabilities)

-- Gets the counts of each unique value in the target feature
-- (dataframe, targetIndex) -> countOfEachUnqiueValue
countTargetValues :: Dataframe -> Int -> [Int]
countTargetValues df targetIndex =
  let targetColumn = getColumn df targetIndex
      targetValues = getColumnUniqueValues df targetIndex
   in map (\v -> length $ filter (== v) targetColumn) targetValues

-- sample =
--   DTNode (\ (age, coughing) -> age <= 30)
--     (LeafNode "Not Sick")
--     (DTNode (\ (age, coughing) -> coughing)
--       (LeafNode "Sick")
--       (LeafNode "Not sick"))

-- navigateTree (80,False) sample
navigateTree :: (a, b) -> DTtree a b -> b
navigateTree dataset (LeafNode b) = b
navigateTree dataset (DTNode p lt rt)
  | p dataset = navigateTree dataset lt
  | otherwise = navigateTree dataset rt

-- convert first 2 elem of list to pairs
convertListToPairs :: [[String]] -> [(String, String)]
convertListToPairs [] = []
convertListToPairs [(x : y : l)] = [(x, y)]
convertListToPairs ((x : y : l) : pairs) = (x, y) : convertListToPairs pairs
