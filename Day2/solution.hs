import System.Environment
import System.Exit
import qualified Data.Vector as V
import Data.List
import Data.Functor.Identity

import Intcode

main = do
  fileName <- getArgs >>= parse
  fileContents <- readFile fileName
  let progn = read ("[" ++ fileContents ++ "]")
  let endValues = map (\(n, v) -> (runProgn' n v progn))
                      [(n,v) | n <- [1..99], v <- [1..99]]
  print $ find (\(res,n,v) -> res == 19690720) endValues


parse :: [String] -> IO String
parse [fileName] = return fileName
parse _ = putStrLn "Wrong number of arguments" >> exitFailure


runProgn' :: Int -> Int -> [Int] -> (Int, Int, Int)
runProgn' noun verb memory = let
  newMemory = setMemory noun verb memory
  Identity (_, Machine finalMemory _ _ _ _) = initializeMachine newMemory ==> runIntcode in
  (V.head finalMemory, noun, verb)


setMemory :: Int -> Int -> [Int] -> [Int]
setMemory noun verb (a:b:c:ds) = a:noun:verb:ds

