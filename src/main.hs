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
    
  

buildModel :: IO (DTtree a b)
buildModel = do 
  getFileNameFromUser <- ask "Please input name of CSV file"

  trainingSet <- catch(readCSVAsDataframe getFileNameFromUser)
                  (\e -> do
                    putStrLn "file does not exist (No such file or directory)"
                    print (e :: IOError)
                    return Nothing)
  
  -- debug
  print trainingSet

  getmaxDepthHyperparam <- ask "Please set your max depth hyperparameter"
  
  let dtree = buildTree (read getmaxDepthHyperparam :: Integer) trainingSet
  return dtree


predict :: DTtree a b -> IO ()
predict currModel = do
  getPredictionFile <- ask "Please input name of CSV file with data for prediction"
  predictionSet <- catch(readCSVAsDataframe getPredictionFile)
                  (\e -> do
                    putStrLn "file does not exist (No such file or directory)"
                    print (e :: IOError)
                    return Nothing)

  let prediction = navigateTree predictionSet currModel
  print prediction
  return ()


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
