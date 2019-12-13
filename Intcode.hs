module Intcode (
  PrognState (PrognState), 
  readPrognState,
  executePrognIO,
  executePrognListIO,
  executeOperListIO,
  semanticAnalysis,
  Operation,
  ListIO,
  isExit,
) where

import System.Exit
import System.IO (hFlush, stdout)
import qualified Data.Vector as V
import Data.List.Split (splitOn)
import Data.Tuple (swap)
import Control.Monad.State

type Ptr = Int

data PrognState = PrognState Memory Ptr deriving (Show)
type Memory = V.Vector Int

data Param = PositionMode | ImmediateMode deriving (Show)

type OpCodeEncoding = Int
type ParamModeEncodings = Int

data Operation = Exit
               | Add Param Param Param
               | Multiply Param Param Param
               | Input Param
               | Output Param
               | JumpIfTrue Param Param
               | JumpIfFalse Param Param
               | TestLessThan Param Param Param
               | TestEqual Param Param Param
               | ParseFailure deriving (Show)

type ListIO = ([Int], [Int])

takeListIO :: State ListIO Int
takeListIO = state $ (\((i:is), os) -> (i, (is, os)))

putListIO :: Int -> State ListIO ()
putListIO o = state $ (\(is, os) -> ((), (is, os ++ [o])))

isExit :: Operation -> Bool
isExit Exit = True
isExit _ = False

listToPrognState :: [Int] -> PrognState
listToPrognState memory = PrognState (V.fromList memory) 0

readMemory :: String -> IO Memory
readMemory fileName = do
  fileContents <- readFile fileName
  return $ V.fromList (map read (splitOn [','] fileContents))

readPrognState :: String -> IO PrognState
readPrognState fileName = do
  memory <- readMemory fileName
  return (PrognState memory 0)

executeProgn :: (Monad m) => (Operation -> PrognState -> m PrognState) -> PrognState -> m PrognState
executeProgn operExecutor prognState = do
  let oper = semanticAnalysis prognState
  if isExit oper
  then return prognState
  else operExecutor oper prognState >>= executeProgn operExecutor

executePrognListIO :: PrognState -> State ListIO PrognState
executePrognListIO = executeProgn executeOperListIO

executeOperListIO :: Operation -> PrognState -> State ListIO PrognState
executeOperListIO (Input param) (PrognState memory ptr) = do
  val <- takeListIO
  let regOut = deref memory param (ptr + 1)
  let newMemory = set memory regOut val
  let newPtr = (ptr + 2)
  return $ PrognState newMemory newPtr
executeOperListIO (Output param) (PrognState memory ptr) = do
  putListIO $ deref memory param (ptr + 1)
  return $ PrognState memory (ptr + 2)
executeOperListIO oper prognState = return $ executeOper oper prognState

executePrognIO :: PrognState -> IO PrognState
executePrognIO = executeProgn executeOperIO

executeOperIO :: Operation -> PrognState -> IO PrognState
executeOperIO (Input param) (PrognState memory ptr) = do
  putStr "Input an Int: "
  hFlush stdout
  val <- readInt
  let regOut = deref memory param (ptr + 1)
  let newMemory = set memory regOut val
  let newPtr = (ptr + 2)
  return $ PrognState newMemory newPtr
executeOperIO (Output param) (PrognState memory ptr) = do
  putStrLn . show $ deref memory param (ptr + 1)
  return $ PrognState memory (ptr + 2)
executeOperIO oper prognState = return $ executeOper oper prognState

executeOper :: Operation -> PrognState -> PrognState
executeOper (Add left right out) prognState = executeBinaryOper (+) left right out prognState
executeOper (Multiply left right out) prognState = executeBinaryOper (*) left right out prognState
executeOper (JumpIfTrue test to) prognState = executeJumpOper (/=0) test to prognState
executeOper (JumpIfFalse test to) prognState = executeJumpOper (==0) test to prognState
executeOper (TestLessThan left right out) prognState = executeBinaryOper (\l r -> fromEnum (l < r)) left right out prognState
executeOper (TestEqual left right out) prognState = executeBinaryOper (\l r -> fromEnum (l == r)) left right out prognState

executeBinaryOper :: (Int -> Int -> Int) -> Param -> Param -> Param -> PrognState -> PrognState
executeBinaryOper intOper left right out (PrognState memory ptr) = let
  valLeft = deref memory left (ptr + 1)
  valRight = deref memory right (ptr + 2)
  regOut = deref memory out (ptr + 3)
  valOut = valLeft `intOper` valRight
  newMemory = set memory regOut valOut
  newPtr = ptr + 4 in
  PrognState newMemory newPtr

executeJumpOper :: (Int -> Bool) -> Param -> Param -> PrognState -> PrognState
executeJumpOper pred test to (PrognState memory ptr) = let
  valTest = deref memory test (ptr + 1)
  newPtr = if pred valTest then deref memory to (ptr + 2) else (ptr + 3) in
  (PrognState memory newPtr)

readInt :: IO Int
readInt = getLine >>= return . read

deref :: Memory -> Param -> Ptr -> Int
deref memory ImmediateMode ptr = getMem memory ptr
deref memory PositionMode ptr = let
  newPtr = deref memory ImmediateMode ptr in
  deref memory ImmediateMode newPtr

getMem :: Memory -> Ptr -> Int
getMem = (V.!)

set :: Memory -> Ptr -> Int -> Memory
set memory ptr val = memory V.// [(ptr, val)]

semanticAnalysis :: PrognState -> Operation
semanticAnalysis (PrognState memory ptr) = let
  rawOpCode = (getMem memory ptr)
  (opCode, paramModes) = parseRawOpCode rawOpCode in
  generateOperation opCode paramModes

parseRawOpCode :: Int -> (OpCodeEncoding, ParamModeEncodings)
parseRawOpCode rawOpCode = swap (rawOpCode `divMod` 100)

generateOperation :: OpCodeEncoding -> ParamModeEncodings -> Operation
generateOperation 99 _ = Exit
generateOperation 1 paramModes = Add (paramModes !? 1) (paramModes !? 2) ImmediateMode
generateOperation 2 paramModes = Multiply (paramModes !? 1) (paramModes !? 2) ImmediateMode
generateOperation 3 _ = Input ImmediateMode
generateOperation 4 paramModes = Output (paramModes !? 1)
generateOperation 5 paramModes = JumpIfTrue (paramModes !? 1) (paramModes !? 2)
generateOperation 6 paramModes = JumpIfFalse (paramModes !? 1) (paramModes !? 2)
generateOperation 7 paramModes = TestLessThan (paramModes !? 1) (paramModes !? 2) ImmediateMode
generateOperation 8 paramModes = TestEqual (paramModes !? 1) (paramModes !? 2) ImmediateMode

(!?) :: ParamModeEncodings -> Int -> Param
(!?) paramModes idx = decodeParamMode (parameterModeN paramModes idx)

parameterModeN :: ParamModeEncodings -> Int -> Int
parameterModeN paramModes idx = paramModes `div` (10^(idx-1)) `mod` 10

decodeParamMode :: Int -> Param
decodeParamMode 0 = PositionMode
decodeParamMode 1 = ImmediateMode
