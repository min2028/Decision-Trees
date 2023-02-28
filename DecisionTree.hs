module DecisionTree where

import Data.List
import Data.Ord
import qualified Data.Map as Map

data DTtree a b = DTNode ((a,b) -> Bool) (DTtree a b) (DTtree a b)
                | LeafNode b

instance (Show a, Show b) => Show (DTtree a b) where
    show (LeafNode b) = show b
    show (DTNode p lt rt) = "<" ++ showFunction p ++ ": " ++ show lt ++ " | " ++ show rt ++ ">"

showFunction :: ((a, b) -> Bool) -> String
showFunction p = "function"

isThisLeafPure :: Eq b => [(a, b)] -> Bool
isThisLeafPure [] = True
isThisLeafPure [(a,b)] = True
isThisLeafPure ((a,b):t) = foldr (\ (x, y) z -> (y == b) && z) True ((a,b):t)

-- Tests
-- isThisLeafPure []
-- isThisLeafPure [(30, 40)]
-- isThisLeafPure [(30, 40), (40, 40)]
-- isThisLeafPure [(30, 40), (40, 50)]
-- isThisLeafPure [(30, "h"), (40, "h")]
-- isThisLeafPure [(30, "y"), (40, "n")]
-- isThisLeafPure [("h", "yes"), ("i", "yes"), ("j", "yes")]


buildTree :: (Eq a, Ord a, Ord b) => Integer -> [(a,b)] -> DTtree a b 
buildTree maxDepth trainingData
    -- if we reached max depth or if the leaf is pure, we can just return LeafNode, 
    -- otherwise create branch for left and right subtree
    | (maxDepth <= 0) || isThisLeafPure trainingData = LeafNode (mostCommonClass trainingData)
    | otherwise = 
        DTNode (\ (x,y) -> x == featureSplittingOn && y <= splitValue)
            (buildTree (maxDepth-1) yesData) 
            (buildTree (maxDepth-1) noData)
                where (featureSplittingOn, splitValue) = nextFeatureToSplit trainingData
                      (yesData, noData) = splitData featureSplittingOn splitValue trainingData
                        
mostCommonClass :: (Ord b) => [(a, b)] -> b
mostCommonClass classes =
    let classCounts = Map.fromListWith (+) [(c, 1) | (_, c) <- classes]
        (maxClass, _) = Map.foldlWithKey' (\(mc, mcCount) c cCount -> if cCount > mcCount then (c, cCount) else (mc, mcCount)) (snd $ head classes, 0) classCounts
    in maxClass

-- stub
nextFeatureToSplit :: (Ord a, Ord b) => [(a, b)] -> (a, b)
nextFeatureToSplit trainingData =
    let (features, values) = unzip trainingData
        featureSet = nub $ sort features
        valueSet = nub $ sort values
        splitValues = [((f,v), computeGiniIndex f v trainingData) | f <- featureSet, v <- valueSet]
        ((bestFeature, bestValue), _) = minimumBy (comparing snd) splitValues
    in (bestFeature, bestValue)

computeGiniIndex :: (Ord a, Ord b) => a -> b -> [(a, b)] -> Double
computeGiniIndex feature value trainingData =
    let (yesData, noData) = splitData feature value trainingData
        totalCount = fromIntegral $ length trainingData
        yesCount = fromIntegral $ length yesData
        noCount = fromIntegral $ length noData
        yesFraction = yesCount / totalCount
        noFraction = noCount / totalCount
        yesGini = computeFractionGini yesData
        noGini = computeFractionGini noData
    in (yesFraction * yesGini) + (noFraction * noGini)

computeFractionGini :: (Ord a, Ord b) => [(a, b)] -> Double
computeFractionGini dataPoints =
    let classCounts = Map.fromListWith (+) [(c, 1) | (_, c) <- dataPoints]
        totalCount = fromIntegral $ length dataPoints
        sumSquaredFractions = sum [fraction * fraction | count <- Map.elems classCounts,
                                                          let fraction = fromIntegral count / totalCount]
    in 1 - sumSquaredFractions

splitData :: (Eq a, Ord b) => a -> b -> [(a, b)] -> ([(a, b)], [(a, b)])
splitData feature value = partition (\(dfeature, dvalue) -> (== feature) dfeature && (<= value) dvalue)

-- Tests
-- splitData "red" "yes" [("red", "yes"), ("red", "yes"), ("green", "no"), ("green", "no"), ("red", "no")]
-- splitData "color" "blue" [("color", "red"), ("color", "blue"), ("shape", "circle"), ("shape", "square")]


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

