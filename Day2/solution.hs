import System.Environment
import System.Exit
import Data.List.Split
import qualified Data.Vector as V
import Data.List

main = do
  fileName <- getArgs >>= parse
  progn <- readProgn fileName
  print $ find (\(n,v) -> (runProgn n v progn) == 19690720)
            [(n,v) | n <- [1..99], v <- [1..99]]


parse :: [String] -> IO String
parse [fileName] = return fileName
parse _ = putStrLn "Wrong number of arguments" >> exitFailure


readProgn :: String -> IO (V.Vector Int)
readProgn fileName = do
  fileContents <- readFile fileName
  return $ V.fromList $ map read (splitOn [','] fileContents)


runProgn :: Int -> Int -> V.Vector Int -> Int
runProgn noun verb progn = V.head $ evaluate 0 $ setProgn noun verb progn


setProgn :: Int -> Int -> V.Vector Int -> V.Vector Int
setProgn noun verb progn = progn V.// [(1,noun), (2,verb)]


evaluate :: Int -> V.Vector Int -> V.Vector Int
evaluate ptr progn = let 
  instructionCode = progn V.! ptr
  newProgn = case instructionCode of
               99 -> progn
               1 -> addInstruction ptr progn
               2 -> multiplyInstruction ptr progn
               _ -> fail "unknown instruction" in
  if instructionCode /= 99
  then evaluate (ptr + 4) newProgn
  else newProgn


instruction :: (Int -> Int -> Int) -> Int -> V.Vector Int -> V.Vector Int
instruction oper ptr progn = let
  inreg1 = progn V.! (ptr + 1)
  inreg2 = progn V.! (ptr + 2)
  inval1 = progn V.! inreg1
  inval2 = progn V.! inreg2
  outval = inval1 `oper` inval2
  outreg = progn V.! (ptr + 3) in
  progn V.// [(outreg, outval)]


addInstruction :: Int -> V.Vector Int -> V.Vector Int
addInstruction = instruction (+)


multiplyInstruction :: Int -> V.Vector Int -> V.Vector Int
multiplyInstruction = instruction (*)
