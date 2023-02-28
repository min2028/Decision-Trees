import Control.Exception
import Data.List
import ReadCsv
import System.Exit
import System.IO
import Text.Read
import DecisionTree
import Dataframe
import Data.Maybe

main :: IO ()
main = do
  mainProgram
  return ()

mainProgram :: IO ()
mainProgram = do
    getFileNameFromUser <- ask "Please input name of CSV file"

    -- Load data from file
    maybeDataFile <- catch(readCSVAsDataframe getFileNameFromUser)
                        (\e -> do
                          putStrLn "file does not exist (No such file or directory)"
                          print (e :: IOError)
                          return Nothing)

    -- Handle missing data file
    let dataFile = case maybeDataFile of
                    Just df -> df
                    Nothing -> error "Data file could not be loaded"

    -- Debug
    print dataFile

    -- Get target variable
    target <- ask "Please specify target variable column name"

    print target

    -- Get maximum depth hyperparameter
    maxDepthInput <- ask "Please set maximum depth hyperparameter (default: 5)"

    -- Handle missing maximum depth
    let maxDepth = readMaybe maxDepthInput :: Maybe Int

    -- Train decision tree
    compFuncInput <- ask "Please set the compare function (==, /=, <=, >=, <, >)"
    let maybeCompFunc = validateInput compFuncInput
    let compFunc = fromMaybe (error "Invalid compare function") maybeCompFunc
    
    let dt = case maxDepth of
                Just md -> trainDecisionTree dataFile target md compFunc
                Nothing -> trainDecisionTree dataFile target 5 compFunc

    -- Debug
    print dt

    -- Ask for input row
    inputRow <- askRow "Please input a row to predict (comma separated)"

    -- Predict outcome for input row
    let input = inputRow
    let prediction = predict dt input dataFile

    -- Display prediction
    case prediction of
        Just p -> putStrLn $ "Prediction: " ++ show p
        Nothing -> putStrLn "Prediction could not be made"
        

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

askRow :: String -> IO [FValue]
askRow q = do
  putStrLn q
  rowStr <- getLine
  let rowList = splitSep (== ',') rowStr
  case traverse readMaybe rowList of
    Just row -> return row
    Nothing -> do
      putStrLn "Invalid input format. Please try again."
      askRow q

validateInput :: String -> Maybe (FValue -> FValue -> Bool)
validateInput "==" = Just (==)
validateInput "/=" = Just (/=)
validateInput "<=" = Just (<=)
validateInput ">=" = Just (>=)
validateInput "<" = Just (<)
validateInput ">" = Just (>)
validateInput _ = Nothing