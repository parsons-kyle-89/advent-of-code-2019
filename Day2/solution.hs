import System.Environment
import System.Exit
import Data.List.Split
import qualified Data.Vector as V

main = do
  fileName <- getArgs >>= parse
  progn <- readProgn fileName >>= resetProgn
  print $ evaluate 0 progn


parse :: [String] -> IO String
parse [fileName] = return fileName
parse _ = putStrLn "Wrong number of arguments" >> exitFailure


readProgn :: String -> IO (V.Vector Int)
readProgn fileName = do
  fileContents <- readFile fileName
  return $ V.fromList $ map read (splitOn [','] fileContents)


resetProgn :: V.Vector Int -> IO (V.Vector Int)
resetProgn progn = return $ progn V.// [(1,12), (2,2)]


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
