import Control.Exception
import Data.List
import RetrieveFile
import System.Exit
import System.IO
import Text.Read
import DecisionTree

main :: IO ()
main = do
  mainProgram
  return ()

mainProgram :: IO ()
mainProgram = do
    getFileNameFromUser <- ask "Please input name of CSV file"

    dataFile <- catch(readFileName getFileNameFromUser)
                    (\e -> do
                      putStrLn "file does not exist (No such file or directory)"
                      print (e :: IOError)
                      return [[""]])
    
    -- debug
    print dataFile

    let dataFilePair = convertListToPairs dataFile

    -- debug
    print dataFilePair

    getmaxDepthHyperparam <- ask "Please set your max depth hyperparameter"
    
    let dtree = buildTree 2 dataFilePair

    getPredictionSet <- ask "Please input name of CSV file with data for prediction"
    predictionFile <- catch(readFileName getPredictionSet)
                    (\e -> do
                      putStrLn "file does not exist (No such file or directory)"
                      print (e :: IOError)
                      return [[""]])
    let predictionPair = convertListToPairs predictionFile
    print predictionPair
    let predictionSet = head predictionPair
    print predictionSet
    
    let prediction = navigateTree predictionSet dtree
    print prediction

    mainProgram

ask :: String -> IO String
ask q =
  do
    putStrLn q
    fname <- getLine
    if fname == "quit"
      then do 
        putStrLn "Exiting program..."
        exitSuccess
      else return fname
