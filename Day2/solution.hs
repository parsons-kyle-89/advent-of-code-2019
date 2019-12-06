import System.Environment
import System.Exit
import qualified Data.Vector as V
import Data.List

import Intcode

main = do
  fileName <- getArgs >>= parse
  progn <- readProgn fileName
  endValues <- mapM (\(n, v) -> do res <- (runProgn' n v progn)
                                   return (res, n, v))
               [(n,v) | n <- [1..99], v <- [1..99]]
  print $ find (\(res,n,v) -> res == 19690720) endValues


parse :: [String] -> IO String
parse [fileName] = return fileName
parse _ = putStrLn "Wrong number of arguments" >> exitFailure


runProgn' :: Int -> Int -> V.Vector Int -> IO Int
runProgn' noun verb progn = do
  let init = setProgn noun verb progn
  final <- runProgn init
  return $ V.head final


setProgn :: Int -> Int -> V.Vector Int -> V.Vector Int
setProgn noun verb progn = progn V.// [(1,noun), (2,verb)]

