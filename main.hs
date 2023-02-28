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
    -- build tree with training set
    model <- buildModel

    -- make predictions on new datasset
    predict model

    -- exit or continue
    nextAction model
    
    
nextAction :: DTtree a b -> IO ()
nextAction model = do
  getNextAction <- ask "Select \'1\' to train another model, \'2\' to make another prediction, \'0\' to exit"
  if getNextAction == "0"
    then exitSuccess
  else if getNextAction == "1"
    then predict model
  else if getNextAction == "2"
    then mainProgram
  else nextAction model

buildModel :: IO (DTtree a b)
buildModel = do 
  getFileNameFromUser <- ask "Please input name of CSV file"

  trainingSet <- catch(readFileName getFileNameFromUser)
                  (\e -> do
                    putStrLn "file does not exist (No such file or directory)"
                    print (e :: IOError)
                    return [[""]])
  
  -- debug
  print trainingSet

  getmaxDepthHyperparam <- ask "Please set your max depth hyperparameter"
  
  let dtree = (LeafNode "Not Sick")
  return dtree

predict :: DTtree a b -> IO ()
predict currModel = do
  getPredictionSet <- ask "Please input name of CSV file with data for prediction"
  predictionFile <- catch(readFileName getPredictionSet)
                  (\e -> do
                    putStrLn "file does not exist (No such file or directory)"
                    print (e :: IOError)
                    return [[""]])

  let prediction = navigateTree predictionSet currModel
  print prediction
  return ()

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
