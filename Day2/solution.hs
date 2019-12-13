import System.Environment
import System.Exit
import qualified Data.Vector as V
import Data.List

import Intcode

main = do
  fileName <- getArgs >>= parse
  prognState <- readPrognState fileName
  endValues <- mapM (\(n, v) -> do res <- (runProgn' n v prognState)
                                   return (res, n, v))
               [(n,v) | n <- [1..99], v <- [1..99]]
  print $ find (\(res,n,v) -> res == 19690720) endValues


parse :: [String] -> IO String
parse [fileName] = return fileName
parse _ = putStrLn "Wrong number of arguments" >> exitFailure


runProgn' :: Int -> Int -> PrognState -> IO Int
runProgn' noun verb (PrognState memory ptr) = do
  let newMemory = setProgn noun verb memory 
  let newPrognState = PrognState newMemory ptr
  (PrognState finalMemory _) <- executePrognIO newPrognState
  return $ V.head finalMemory


setProgn :: Int -> Int -> V.Vector Int -> V.Vector Int
setProgn noun verb progn = progn V.// [(1,noun), (2,verb)]

