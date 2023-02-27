import Control.Exception
import Data.List
import RetrieveFile
import System.Exit
import System.IO
import Text.Read

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

    getmaxDepthHyperparam <- ask "Please set your max depth hyperparameter"

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
