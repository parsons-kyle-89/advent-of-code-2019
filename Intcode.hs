module Intcode (runProgn, readProgn) where

import System.IO (hFlush, stdout)
import System.Exit (exitSuccess, exitFailure)
import qualified Data.Vector as V
import Data.List.Split (splitOn)

data Progn = Progn (V.Vector Int) deriving (Show)
type Ptr = Int
type Instruction = Ptr -> ParameterModes -> Progn -> IO (Progn, Int)
data ParameterMode = PositionMode | ImmediateMode
type ParameterModes = [ParameterMode]

setValue :: Progn -> Ptr -> Int -> Progn
setValue (Progn code) ptr val = Progn (code V.// [(ptr, val)])

(!?) :: ParameterModes -> Int -> ParameterMode
(!?) parameterModes idx = 
  if idx <= length parameterModes
  then parameterModes !! (idx - 1)
  else PositionMode

readProgn :: String -> IO Progn
readProgn fileName = do
  fileContents <- readFile fileName
  return $ Progn (V.fromList (map read (splitOn [','] fileContents)))

runProgn :: Progn -> IO Progn
runProgn = evaluate 0

evaluate :: Ptr -> Progn -> IO Progn
evaluate ptr progn = do
  let (instructionCode, parameterModes) = parseOpCode progn ptr
  let instruction = getInstruction instructionCode
  (newProgn, newPtr) <- instruction ptr parameterModes progn
  if instructionCode /= 99 then evaluate newPtr newProgn else return newProgn


getParam :: ParameterModes -> Progn -> Ptr -> Int -> Int
getParam parameterModes progn ptr idx = let
  paramptr = getPtr progn (ptr + idx)
  parameterMode = parameterModes !? idx in
  deref parameterMode progn paramptr

getPtr :: Progn -> Ptr -> Ptr
getPtr = deref PositionMode

deref :: ParameterMode -> Progn -> Ptr -> Int
deref ImmediateMode _ ptr = ptr
deref PositionMode (Progn code) ptr = code V.! ptr

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
getInstruction 99 = exit
getInstruction 1 = add
getInstruction 2 = multiply
getInstruction 3 = input
getInstruction 4 = output
getInstruction 5 = jumpIfTrue
getInstruction 6 = jumpIfFalse
getInstruction 7 = lessThanTest
getInstruction 8 = equalsTest
getInstruction _ = failInstruction

operInstruction :: (Int -> Int -> Int) -> Instruction
operInstruction oper ptr parameterModes progn = let
  inval1 = getParam parameterModes progn ptr 1
  inval2 = getParam parameterModes progn ptr 2
  outptr = getPtr progn (ptr + 3)
  outval = inval1 `oper` inval2 in
  return (setValue progn outptr outval, ptr + 4)

add :: Instruction
add = operInstruction (+)

multiply :: Instruction
multiply = operInstruction (*)

exit ::  Instruction
exit ptr _ progn = return (progn, ptr + 1)

failInstruction :: Instruction
failInstruction _ _ progn = exitFailure

input :: Instruction
input ptr _ progn = do
  putStr "Input an Int: "
  hFlush stdout
  input <- readInt
  let outreg = getPtr progn (ptr + 1)
  return (setValue progn outreg input, ptr + 2)

readInt :: IO Int
readInt = getLine >>= return . read

output :: Instruction
output ptr parameterModes progn = do
  let outval = getParam parameterModes progn ptr 1
  print outval
  return (progn, ptr + 2)

jumpIfTestInstruction :: (Int -> Bool) -> Instruction
jumpIfTestInstruction predicate ptr parameterModes progn = let
  testval = getParam parameterModes progn ptr 1
  outptrval = getParam parameterModes progn ptr 2
  newptr = if predicate testval then outptrval else (ptr + 3) in
  return (progn, newptr)

jumpIfTrue :: Instruction
jumpIfTrue = jumpIfTestInstruction (/=0)

jumpIfFalse :: Instruction
jumpIfFalse = jumpIfTestInstruction (==0)

testInstruction :: (Int -> Int -> Bool) -> Instruction
testInstruction bipredicate ptr parameterModes progn = let
  val1 = getParam parameterModes progn ptr 1
  val2 = getParam parameterModes progn ptr 2
  outptr = getPtr progn (ptr + 3)
  outval = if bipredicate val1 val2 then 1 else 0 in
  return (setValue progn outptr outval, ptr + 4)

lessThanTest :: Instruction
lessThanTest = testInstruction (<)

equalsTest :: Instruction
equalsTest = testInstruction (==)
