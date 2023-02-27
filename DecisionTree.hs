import Data.List
import Data.Ord
import qualified Data.Map as Map

data DTtree a b = DTNode ((a,b) -> Bool) (DTtree a b) (DTtree a b)
                | LeafNode b

-- instance (show b) => show (DTtree a b) where
--     show LeafNode b = b

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


buildTree :: (Eq a, Ord b) => Integer -> [(a,b)] -> DTtree a b 
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
nextFeatureToSplit :: [(a, b)] -> (a, b)
nextFeatureToSplit dataPoints = head dataPoints

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

