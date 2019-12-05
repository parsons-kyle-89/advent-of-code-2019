module Intcode (evaluate, readProgn) where

import System.Exit (exitSuccess, exitFailure)
import qualified Data.Vector as V
import Data.List.Split (splitOn)

type Progn = V.Vector Int
type Instruction = Int -> ParameterModes -> Progn -> IO (Progn, Int)
data ParameterMode = PositionMode | ImmediateMode
type ParameterModes = [ParameterMode]

(!?) :: ParameterModes -> Int -> ParameterMode
(!?) parameterModes idx = 
  if idx < length parameterModes
  then parameterModes !! idx
  else PositionMode

readProgn :: String -> IO Progn
readProgn fileName = do
  fileContents <- readFile fileName
  return $ V.fromList $ map read (splitOn [','] fileContents)

evaluate :: Int -> Progn -> IO Progn
evaluate ptr progn = do
  let (instructionCode, parameterModes) = parseOpCode progn ptr
  let instruction = getInstruction instructionCode
  (newProgn, step) <- instruction ptr parameterModes progn
  if instructionCode /= 99
  then evaluate (ptr + step) newProgn
  else return newProgn


deref :: ParameterMode -> Progn -> Int -> Int
deref ImmediateMode _ ptr = ptr
deref PositionMode progn ptr = progn V.! ptr

parseOpCode :: Progn -> Int -> (Int, ParameterModes)
parseOpCode progn ptr = let
  rawOpCode = deref PositionMode progn ptr
  (parameterModesInt, opCode) = rawOpCode `divMod` 100
  parameterModes = parseParameterModes parameterModesInt in
  (opCode, parameterModes)

parseParameterModes :: Int -> ParameterModes
parseParameterModes parameterModesInt =
  map parseParameterMode $ reverse (show parameterModesInt)

parseParameterMode :: Char -> ParameterMode
parseParameterMode '0' = PositionMode
parseParameterMode '1' = ImmediateMode

getInstruction :: Int -> Instruction
getInstruction 99 = exitInstruction
getInstruction 1 = addInstruction
getInstruction 2 = multiplyInstruction
getInstruction 3 = inputInstruction
getInstruction 4 = outputInstruction
getInstruction _ = failInstruction

operInstruction :: (Int -> Int -> Int) -> Instruction
operInstruction oper ptr parameterModes progn = let
  ptr1 = progn V.! (ptr + 1)
  ptr2 = progn V.! (ptr + 2)
  outptr = progn V.! (ptr + 3)
  inval1 = deref (parameterModes !? 0) progn ptr1
  inval2 = deref (parameterModes !? 1) progn ptr2
  outval = inval1 `oper` inval2 in
  return (progn V.// [(outptr, outval)], 4)

addInstruction :: Instruction
addInstruction = operInstruction (+)

multiplyInstruction :: Instruction
multiplyInstruction = operInstruction (*)

exitInstruction ::  Instruction
exitInstruction _ _ progn = return (progn, 1)

failInstruction :: Instruction
failInstruction _ _ progn = exitFailure

inputInstruction :: Instruction
inputInstruction ptr _ progn = do
  putStr "Input an Int: "
  input <- readInt
  let outreg = progn V.! (ptr + 1)
  return (progn V.// [(outreg, input)], 2)

readInt :: IO Int
readInt = getLine >>= return . read

outputInstruction :: Instruction
outputInstruction ptr parameterModes progn = do
  let outptr = progn V.! (ptr + 1)
  let outval = deref (parameterModes !? 0) progn outptr
  print outval
  return (progn, 2)
