module RetrieveFile where

import System.IO
import Text.Read
import Data.List

splitsep :: (a -> Bool) -> [a] -> [[a]]
splitsep c [] = [[]]
splitsep c (h:t)
    | c h = [] : splitsep c t
    | otherwise = (h:word):otherWords
        where (word:otherWords) = splitsep c t


-- Tests
-- splitsep (==',') "12,2,3,4,5"
-- splitsep (==',') "testing,to,see,if,it,works"
-- splitsep (=='/') "abc/def/ghi//,."
-- splitsep even [1..10]
-- splitsep (\x -> (x `mod` 4) == 0) [1..20]

readFileName :: FilePath -> IO [[String]]
readFileName filename = 
    do
        file <- readFile filename


        let matrix = [splitsep (==',') lines | lines <- splitsep (=='\n') file]
        let transMat = transpose (tail matrix)
        return transMat
