data DTtree a b = DTNode (a -> Bool) (DTtree a b) (DTtree a b)
                | LeafNode b

-- instance (show b) => show (DTtree a b) where
--     show LeafNode b = b


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

