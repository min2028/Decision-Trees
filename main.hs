import System.IO
import Text.Read
import Data.List
import Control.Exception

import RetrieveFile


main :: IO ()
main = do
    mainProgram
    return ()
    

mainProgram :: IO ()
mainProgram = do
    getFileNameFromUser <- ask("Please input name of CSV file")

    dataFile <- catch(readFileName getFileNameFromUser)
                    (\e -> do
                      putStrLn "file does not exist (No such file or directory)"
                      putStrLn (show (e :: IOError))
                      return [[""]])
    
    -- debug
    putStrLn (show dataFile)

    getmaxDepthHyperparam <- ask("Please set your max depth hyperparameter")

    mainProgram

ask :: String -> IO String
ask q =
    do
        putStrLn q
        fname <- getLine
        return fname
