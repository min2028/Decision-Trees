module DecisionTree (
  trainDecisionTree,
  DecisionTree (..)
) where

import Data.List
import qualified Data.Map as Map
import Data.Ord
import Dataframe

data DecisionTree a
  = Leaf a
  | Node String FValue (DecisionTree a) (DecisionTree a)

instance (Show a) => Show (DecisionTree a) where
  show (Leaf a) =
    "\nLeaf " ++ show a
  show (Node col val left right) =
    "\nNode " ++ show col ++ " " ++ show val ++ " (" ++ show left ++ ") (" ++ show right ++ ")"

-- trains the decision tree
-- (dataframe, targetHeader, maxDepth)
trainDecisionTree :: Dataframe -> String -> Int -> (FValue -> FValue -> Bool) -> DecisionTree FValue
trainDecisionTree df target maxDepth compFunc = trainDecisionTree' df target maxDepth compFunc 0

-- private helper with accumulator to track depth
trainDecisionTree' :: Dataframe -> String -> Int -> (FValue -> FValue -> Bool) -> Int -> DecisionTree FValue
trainDecisionTree' df target maxDepth compFunc depth
  | allEqual targetValues = Leaf (head targetValues)
  | null $ headers df = Leaf (majorityValue targetValues)
  | depth >= maxDepth = Leaf (majorityValue targetValues)
  | otherwise =
    Node
      (headers df !! featureIndex)
      value
      leftTree
      rightTree
  where
    numFeatures = getWidth df - 1
    targetIndex = getColumnIndex (headers df) target
    targetValues = getColumn df targetIndex
    leftTree = if getLength yesDf == 0 then Leaf (head targetValues) else trainDecisionTree' yesDf target maxDepth compFunc (depth + 1)
    rightTree = if getLength noDf == 0 then Leaf(head targetValues) else trainDecisionTree' noDf target maxDepth compFunc (depth + 1)
    (featureIndex, value, (yesDf, noDf)) = fst $ minimumBy (comparing snd) splitScores
    splitScores = [(splitByValue, computeGiniIndex splitByValue) | splitByValue <- splitDfs]
    splitDfs = concat [getPossibleSplits df targetIndex columnIndex compFunc | columnIndex <- [0 .. numFeatures]]

-- Check if all elements in a list are the same
allEqual :: (Eq a) => [a] -> Bool
allEqual [] = True
allEqual (x:xs) = all (== x) xs

-- Returns the value that has the highest frequncy in the list
majorityValue :: (Eq a, Ord a) => [a] -> a
majorityValue [] =
  error "majorityValue: list is empty"
majorityValue xs =
  snd $ maximumBy compareFreqs freqs
  where
    freqs = [(length group, head group) | group <- group $ sort xs]
    compareFreqs (freq1, _) (freq2, _) = compare freq1 freq2

-- Computes the GINI index for a given column
-- (featureIndex, valueSpltOn, (yesDataframe, noDataframe)) -> giniIndex
computeGiniIndex :: (Int, FValue, (Dataframe, Dataframe)) -> Double
computeGiniIndex (featureIndex, _, (yesDf, noDf)) =
  let totalCount = yesCount + noCount
      yesCount = fromIntegral $ getLength yesDf
      noCount = fromIntegral $ getLength noDf
      yesFraction = yesCount / totalCount
      noFraction = noCount / totalCount
      yesGini = computeFractionGini yesDf featureIndex
      noGini = computeFractionGini noDf featureIndex
      gini = (yesFraction * yesGini) + (noFraction * noGini)
   in gini

-- Gets all possible splits for each column
-- (dataframe, targetIndex, featureIndex) -> [(featureIndex, valueSpltOn, (yesDataframe, noDataframe))]
getPossibleSplits :: Dataframe -> Int -> Int -> (FValue -> FValue -> Bool) -> [(Int, FValue, (Dataframe, Dataframe))]
getPossibleSplits df targetIndex featureIndex compFunc = [(featureIndex, value, splitDataframe df featureIndex compFunc value) | value <- getColumnUniqueValues df featureIndex]

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
   in map (\v -> length $ filter (==v) targetColumn) targetValues

-- Splits a dataframe into two dataframes based on the column, String, and value, FValue
-- (dataframe, columnIndex, value)
splitDataframe :: Dataframe -> Int -> (FValue -> FValue -> Bool) -> FValue -> (Dataframe, Dataframe)
splitDataframe (Dataframe hs cs rs) colIndex compFunc val = (Dataframe hs cs yesRows, Dataframe hs cs noRows)
  where
    yesRows = filter (\row -> compFunc (row !! colIndex) val) rs
    noRows = filter (\row -> not (compFunc (row !! colIndex) val)) rs
