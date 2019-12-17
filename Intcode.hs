{-# language RankNTypes, FlexibleContexts #-}

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
  runIntcodeIO,
  runIntcode,
  initializeMachine,
  Machine (Machine),
  (==>),
) where

import System.Exit
import System.IO (hFlush, stdout)
import qualified Data.Vector as V
import Data.List.Split (splitOn)
import Data.Tuple (swap)
import Control.Monad.State
import Data.Functor.Identity (Identity)
import Control.Monad.Trans.State (StateT)

data PrognState = PrognState Memory Ptr Ptr deriving (Show)
data Machine = Machine Memory Ptr InputQueue OutputQueue deriving (Show)
type MachineState m a = (Monad m) => StateT Machine m a
type Ptr = Int
type Memory = V.Vector Int
type InputQueue = [Int]
type OutputQueue = [Int]

data Param = PositionMode | ImmediateMode | RelativeMode deriving (Show)

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
               | ShiftBase Param
               | ParseFailure deriving (Show)

data IntcodeResponse = Value Int | NeedsInput | ProgramExit deriving (Show)

type ListIO = ([Int], [Int])

takeListIO :: State ListIO Int
takeListIO = state $ (\((i:is), os) -> (i, (is, os)))

putListIO :: Int -> State ListIO ()
putListIO o = state $ (\(is, os) -> ((), (is, os ++ [o])))

isExit :: Operation -> Bool
isExit Exit = True
isExit _ = False

listToPrognState :: [Int] -> PrognState
listToPrognState memory = PrognState (V.fromList memory) 0 0

readMemory :: String -> IO Memory
readMemory fileName = do
  fileContents <- readFile fileName
  return $ V.fromList (map read (splitOn [','] fileContents))

readPrognState :: String -> IO PrognState
readPrognState fileName = do
  memory <- readMemory fileName
  return (PrognState memory 0 0)

executeProgn :: (Monad m) => (Operation -> PrognState -> m PrognState) -> PrognState -> m PrognState
executeProgn operExecutor prognState = do
  let oper = semanticAnalysis prognState
  if isExit oper
  then return prognState
  else operExecutor oper prognState >>= executeProgn operExecutor

executePrognListIO :: PrognState -> State ListIO PrognState
executePrognListIO = executeProgn executeOperListIO

executeOperListIO :: Operation -> PrognState -> State ListIO PrognState
executeOperListIO (Input param) (PrognState memory ptr base) = do
  val <- takeListIO
  let newMemory = set memory param (ptr + 1) base val
  let newPtr = (ptr + 2)
  return $ PrognState newMemory newPtr base
executeOperListIO (Output param) (PrognState memory ptr base) = do
  putListIO $ deref memory param (ptr + 1) base
  return $ PrognState memory (ptr + 2) base
executeOperListIO oper prognState = return $ executeOper oper prognState

executePrognIO :: PrognState -> IO PrognState
executePrognIO = executeProgn executeOperIO

executeOperIO :: Operation -> PrognState -> IO PrognState
executeOperIO (Input param) (PrognState memory ptr base) = do
  putStr "Input an Int: "
  hFlush stdout
  val <- readInt
  let newMemory = set memory param (ptr + 1) base val
  let newPtr = (ptr + 2)
  return $ PrognState newMemory newPtr base
executeOperIO (Output param) (PrognState memory ptr base) = do
  putStrLn . show $ deref memory param (ptr + 1) base
  return $ PrognState memory (ptr + 2) base
executeOperIO oper prognState = return $ executeOper oper prognState

executeOper :: Operation -> PrognState -> PrognState
executeOper (Add left right out) prognState = executeBinaryOper (+) left right out prognState
executeOper (Multiply left right out) prognState = executeBinaryOper (*) left right out prognState
executeOper (JumpIfTrue test to) prognState = executeJumpOper (/=0) test to prognState
executeOper (JumpIfFalse test to) prognState = executeJumpOper (==0) test to prognState
executeOper (TestLessThan left right out) prognState = executeBinaryOper (\l r -> fromEnum (l < r)) left right out prognState
executeOper (TestEqual left right out) prognState = executeBinaryOper (\l r -> fromEnum (l == r)) left right out prognState
executeOper (ShiftBase by) (PrognState memory ptr base) = let
  shiftBy = deref memory by (ptr + 1) base
  newBase = base + shiftBy
  newPtr = ptr + 2 in
  PrognState memory newPtr newBase

executeBinaryOper :: (Int -> Int -> Int) -> Param -> Param -> Param -> PrognState -> PrognState
executeBinaryOper intOper left right out (PrognState memory ptr base) = let
  valLeft = deref memory left (ptr + 1) base
  valRight = deref memory right (ptr + 2) base
  valOut = valLeft `intOper` valRight
  newMemory = set memory out (ptr + 3) base valOut
  newPtr = ptr + 4 in
  PrognState newMemory newPtr base

executeJumpOper :: (Int -> Bool) -> Param -> Param -> PrognState -> PrognState
executeJumpOper pred test to (PrognState memory ptr base) = let
  valTest = deref memory test (ptr + 1) base
  newPtr = if pred valTest then deref memory to (ptr + 2) base else (ptr + 3) in
  (PrognState memory newPtr base)

readInt :: IO Int
readInt = getLine >>= return . read

deref :: Memory -> Param -> Ptr -> Ptr -> Int
deref memory ImmediateMode ptr base = let
  expandedMemory = expandMemory ptr memory in
  expandedMemory V.! ptr
deref memory PositionMode ptr base = let
  newPtr = deref memory ImmediateMode ptr base in
  deref memory ImmediateMode newPtr base
deref memory RelativeMode ptr base = let
  newPtr = (deref memory ImmediateMode ptr base) + base in
  deref memory ImmediateMode newPtr base

set :: Memory -> Param -> Ptr -> Ptr -> Int -> Memory
set memory PositionMode ptr base val = let
  outReg = deref memory ImmediateMode ptr base
  expandedMemory = expandMemory outReg memory in
  expandedMemory V.// [(outReg, val)]
set memory RelativeMode ptr base val = let
  outReg = (deref memory ImmediateMode ptr base) + base 
  expandedMemory = expandMemory outReg memory in
  expandedMemory V.// [(outReg, val)]

expandMemory :: Ptr -> Memory -> Memory
expandMemory ptr memory = let
  paddingLength = max (ptr - length memory + 1) 0 in
  memory V.++ V.fromList (replicate paddingLength 0)

semanticAnalysis :: PrognState -> Operation
semanticAnalysis (PrognState memory ptr base) = let
  rawOpCode = deref memory ImmediateMode ptr base
  (opCode, paramModes) = parseRawOpCode rawOpCode in
  generateOperation opCode paramModes

parseRawOpCode :: Int -> (OpCodeEncoding, ParamModeEncodings)
parseRawOpCode rawOpCode = swap (rawOpCode `divMod` 100)

generateOperation :: OpCodeEncoding -> ParamModeEncodings -> Operation
generateOperation 99 _ = Exit
generateOperation 1 paramModes = Add (paramModes !? 1) (paramModes !? 2) (paramModes !? 3)
generateOperation 2 paramModes = Multiply (paramModes !? 1) (paramModes !? 2) (paramModes !? 3)
generateOperation 3 paramModes = Input (paramModes !? 1)
generateOperation 4 paramModes = Output (paramModes !? 1)
generateOperation 5 paramModes = JumpIfTrue (paramModes !? 1) (paramModes !? 2)
generateOperation 6 paramModes = JumpIfFalse (paramModes !? 1) (paramModes !? 2)
generateOperation 7 paramModes = TestLessThan (paramModes !? 1) (paramModes !? 2) (paramModes !? 3)
generateOperation 8 paramModes = TestEqual (paramModes !? 1) (paramModes !? 2) (paramModes !? 3)
generateOperation 9 paramModes = ShiftBase (paramModes !? 1)

(!?) :: ParamModeEncodings -> Int -> Param
(!?) paramModes idx = decodeParamMode (parameterModeN paramModes idx)

parameterModeN :: ParamModeEncodings -> Int -> Int
parameterModeN paramModes idx = paramModes `div` (10^(idx-1)) `mod` 10

decodeParamMode :: Int -> Param
decodeParamMode 0 = PositionMode
decodeParamMode 1 = ImmediateMode
decodeParamMode 2 = RelativeMode

(==>) :: s -> StateT s m a -> m (a, s)
(==>) initialState stateFunc = runStateT stateFunc initialState
infix 0 ==>

initializeMachine :: [Int] -> Machine
initializeMachine = initializeMachineFromMemory . V.fromList

initializeMachineFromMemory :: Memory -> Machine
initializeMachineFromMemory memory = Machine memory 0 [] []

getMemory :: MachineState m Memory
getMemory = state $ \ms@(Machine memory _ _ _) -> (memory, ms)

getPtr :: MachineState m Ptr
getPtr = state $ \ms@(Machine _ ptr _ _) -> (ptr, ms)

getInputQueue :: MachineState m InputQueue
getInputQueue = state $ \ms@(Machine _ _ inputQueue _) -> (inputQueue, ms)

getOutputQueue :: MachineState m OutputQueue
getOutputQueue = state $ \ms@(Machine _ _ _ outputQueue) -> (outputQueue, ms)

setPtr :: Ptr -> MachineState m ()
setPtr newPtr = state $ \(Machine m ptr iQ oQ) -> ((), Machine m newPtr iQ oQ)

setMemory :: Memory -> MachineState m ()
setMemory newMemory =
  state $ \(Machine memory p iQ oQ) -> ((), Machine newMemory p iQ oQ)

setInputQueue :: InputQueue -> MachineState m ()
setInputQueue newInputQueue =
  state $ \(Machine m p inputQueue oQ) -> ((), Machine m p newInputQueue oQ)

setOutputQueue :: OutputQueue -> MachineState m ()
setOutputQueue newOutputQueue =
  state $ \(Machine m p iQ outputQueue) -> ((), Machine m p iQ newOutputQueue)

jumpTo :: Ptr -> MachineState m ()
jumpTo = setPtr

incPtr :: Int -> MachineState m ()
incPtr by = do
  ptr <- getPtr
  jumpTo (ptr + by)

enqueueInput :: Int -> MachineState m ()
enqueueInput input = do
  inputQueue <- getInputQueue
  setInputQueue (inputQueue ++ [input])

dequeueInput :: MachineState m (Maybe Int)
dequeueInput = do
  inputQueue <- getInputQueue
  case inputQueue of
    [] -> return $ Nothing
    i:is -> do setInputQueue is
               return $ Just i

enqueueOutput :: Int -> MachineState m ()
enqueueOutput output = do
  outputQueue <- getOutputQueue
  setOutputQueue (outputQueue ++ [output])
  
dequeueOutput :: MachineState m (Maybe Int)
dequeueOutput = do
  outputQueue <- getOutputQueue
  case outputQueue of
    [] -> return $ Nothing
    o:os -> do setOutputQueue os
               return $ Just o

getAtPtr :: Ptr -> MachineState m Int
getAtPtr ptr = do
  memory <- getMemory
  return $ memory V.! ptr

getAtCurrentPtr :: MachineState m Int
getAtCurrentPtr = do
  ptr <- getPtr
  getAtPtr ptr

derefAtPtr :: Param -> Ptr -> MachineState m Int
derefAtPtr ImmediateMode ptr = getAtPtr ptr
derefAtPtr PositionMode ptr = do
  reference <- derefAtPtr ImmediateMode ptr
  derefAtPtr ImmediateMode reference

setAtPtr :: Ptr -> Int -> MachineState m ()
setAtPtr ptr val = do
  memory <- getMemory
  let newMemory = memory V.// [(ptr, val)]
  setMemory newMemory

setrefAtPtr :: Param -> Ptr -> Int -> MachineState m ()
setrefAtPtr PositionMode ptr val = do
  reference <- derefAtPtr ImmediateMode ptr
  setAtPtr reference val

getCurrentOper :: MachineState m Operation
getCurrentOper = do
  rawOpCode <- getAtCurrentPtr
  let (opCode, paramModes) = parseRawOpCode rawOpCode
  return $ generateOperation opCode paramModes

inputValue :: Int -> MachineState m ()
inputValue = enqueueInput

data NextStep = Advance | GetInput | RaiseExit

getNextStep :: MachineState m NextStep
getNextStep = do
  currentOper <- getCurrentOper
  inputQueue <- getInputQueue
  case (currentOper, inputQueue) of
    (Exit, _) -> return $ RaiseExit
    (Input _, []) -> return $ GetInput
    otherwise -> return $ Advance

getValue :: MachineState m IntcodeResponse
getValue = do
  maybeOutput <- dequeueOutput
  case maybeOutput of
    Just value -> return $ Value value
    Nothing -> do
      nextStep <- getNextStep
      case nextStep of
        GetInput -> return $ NeedsInput
        RaiseExit -> return $ ProgramExit
        Advance -> do
          advance
          getValue

advance :: MachineState m ()
advance = do
  currentOper <- getCurrentOper
  executeOperation currentOper

advanceToEnd :: MachineState m ()
advanceToEnd = do
  currentOper <- getCurrentOper
  case currentOper of
    Exit -> return ()
    otherwise -> do
      advance
      advanceToEnd

data GenericBinaryOperation = GBO Param Param Param
data GenericJumpOperation = GJO Param Param

executeOperation :: Operation -> MachineState m ()
executeOperation (Add left right out) = executeBinaryOperation (+) (GBO left right out)
executeOperation (Multiply left right out) = executeBinaryOperation (*) (GBO left right out)
executeOperation (Input param) = do
  ptr <- getPtr
  Just val <- dequeueInput
  setrefAtPtr param (ptr + 1) val
  incPtr 2
executeOperation (Output param) = do
  ptr <- getPtr
  val <- derefAtPtr param (ptr + 1)
  enqueueOutput val
  incPtr 2
executeOperation (JumpIfTrue test to) = executeJumpOperation (/=0) (GJO test to)
executeOperation (JumpIfFalse test to) = executeJumpOperation (==0) (GJO test to)
executeOperation (TestLessThan left right out) =
  executeBinaryOperation (\l r -> fromEnum (l < r)) (GBO left right out)
executeOperation (TestEqual left right out) =
  executeBinaryOperation (\l r -> fromEnum (l == r)) (GBO left right out)
-- executeOper (ShiftBase by) (PrognState memory ptr base) = let
--   shiftBy = deref memory by (ptr + 1) base
--   newBase = base + shiftBy
--   newPtr = ptr + 2 in
--   PrognState memory newPtr newBase

executeBinaryOperation :: (Int -> Int -> Int) -> GenericBinaryOperation -> MachineState m ()
executeBinaryOperation intOper (GBO left right out) = do
  ptr <- getPtr
  valLeft <- derefAtPtr left (ptr + 1)
  valRight <- derefAtPtr right (ptr + 2)
  let valOut = valLeft `intOper` valRight
  setrefAtPtr out (ptr + 3) valOut
  incPtr 4

executeJumpOperation :: (Int -> Bool) -> GenericJumpOperation -> MachineState m ()
executeJumpOperation pred (GJO test to) = do
  ptr <- getPtr
  valTest <- derefAtPtr test (ptr + 1)
  newPtr <- if pred valTest then derefAtPtr to (ptr + 2) else return (ptr + 3)
  setPtr newPtr

runIntcodeIO :: MachineState IO ()
runIntcodeIO = do
  intcodeResponse <- getValue
  case intcodeResponse of
    ProgramExit -> return ()
    NeedsInput -> do
      liftIO $ putStr "input value: "
      liftIO $ hFlush stdout
      input <- liftIO readInt
      inputValue input
      runIntcodeIO
    Value val -> do
      liftIO $ putStrLn ("output value: " ++ show val)
      runIntcodeIO

runIntcode :: MachineState Identity ()
runIntcode = advanceToEnd
