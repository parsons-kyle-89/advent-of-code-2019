import System.Environment (getArgs)
import System.Exit

import Intcode

main = do
  fileName <- getArgs >>= parseArgs
  prognState <- readPrognState fileName
  executePrognIO prognState

parseArgs :: [String] -> IO String
parseArgs [fileName] = return fileName
parseArgs _ = putStrLn "Wrong number of arguments" >> exitFailure
