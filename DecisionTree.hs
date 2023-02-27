import Data.List

data DTtree a b = DTNode (a -> Bool) (DTtree a b) (DTtree a b)
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


-- buildTree :: Integer -> [(a,b)] -> DTtree a b 
-- buildTree maxDepth trainingData
--     -- if we reached max depth or if the leaf is pure, we can just return LeafNode, 
--     -- otherwise create branch for left and right subtree
--     | (currDepth >= maxDepth) || isThisLeafPure trainingData = LeafNode (mostCommonClass trainingData)
--     | otherwise = 
--         DTNode (a -> Bool) 
--             (buildTree (currDepth+1) yesData) 
--             (buildTree (currDepth+1) noData)
--                 where (featureSplitting, splitValue) = nextFeatureToSplit trainingData
--                         (yesData, noData) = splitData feature splitValue trainingData
                        
 


    


sample =
  DTNode (\(age, coughing) -> age <= 30)
    (LeafNode "Not Sick")
    (DTNode (\(age, coughing) -> coughing)
      (LeafNode "Sick") 
      (LeafNode "Not sick"))

-- navigateTree (80,False) sample
navigateTree dataset (LeafNode b) = b
navigateTree dataset (DTNode p lt rt)
    | p dataset = navigateTree dataset lt
    | otherwise = navigateTree dataset rt

