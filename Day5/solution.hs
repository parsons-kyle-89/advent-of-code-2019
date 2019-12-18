import System.Environment (getArgs)
import System.Exit

import Intcode

main = do
  fileName <- getArgs >>= parseArgs
  fileContents <- readFile fileName
  let progn = read ("[" ++ fileContents ++ "]")
  initializeMachine progn ==> runIntcodeIO

parseArgs :: [String] -> IO String
parseArgs [fileName] = return fileName
parseArgs _ = putStrLn "Wrong number of arguments" >> exitFailure
