{-# language RankNTypes, FlexibleContexts #-}

module Intcode (
  runIntcodeIO,
  runIntcode,
  initializeMachine,
  Machine (Machine),
  IntcodeResponse (Value, NeedsInput, ProgramExit), 
  inputValue,
  getValue,
  (==>),
) where

import System.IO (hFlush, stdout)
import qualified Data.Vector as V
import Data.Tuple (swap)
import Data.Functor.Identity (Identity)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State (StateT, state, runStateT)

data Machine = Machine Memory Ptr Ptr InputQueue OutputQueue deriving (Show)
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

(==>) :: s -> StateT s m a -> m (a, s)
(==>) initialState stateFunc = runStateT stateFunc initialState
infix 0 ==>

initializeMachine :: [Int] -> Machine
initializeMachine = initializeMachineFromMemory . V.fromList

initializeMachineFromMemory :: Memory -> Machine
initializeMachineFromMemory memory = Machine memory 0 0 [] []

getMemory :: MachineState m Memory
getMemory = state $ \ms@(Machine memory _ _ _ _) -> (memory, ms)

getPtr :: MachineState m Ptr
getPtr = state $ \ms@(Machine _ ptr _ _ _) -> (ptr, ms)

getBase :: MachineState m Ptr
getBase = state $ \ms@(Machine _ _ base _ _) -> (base, ms)

getInputQueue :: MachineState m InputQueue
getInputQueue = state $ \ms@(Machine _ _ _ inputQueue _) -> (inputQueue, ms)

getOutputQueue :: MachineState m OutputQueue
getOutputQueue = state $ \ms@(Machine _ _ _ _ outputQueue) -> (outputQueue, ms)

setMemory :: Memory -> MachineState m ()
setMemory newMemory =
  state $ \(Machine memory p b iQ oQ) -> ((), Machine newMemory p b iQ oQ)

setPtr :: Ptr -> MachineState m ()
setPtr newPtr = state $ \(Machine m ptr b iQ oQ) -> ((), Machine m newPtr b iQ oQ)

setBase :: Ptr -> MachineState m ()
setBase newBase = state $ \(Machine m p base iQ oQ) -> ((), Machine m p newBase iQ oQ)

setInputQueue :: InputQueue -> MachineState m ()
setInputQueue newInputQueue =
  state $ \(Machine m p b inputQueue oQ) -> ((), Machine m p b newInputQueue oQ)

setOutputQueue :: OutputQueue -> MachineState m ()
setOutputQueue newOutputQueue =
  state $ \(Machine m p b iQ outputQueue) -> ((), Machine m p b iQ newOutputQueue)

jumpTo :: Ptr -> MachineState m ()
jumpTo = setPtr

incPtr :: Int -> MachineState m ()
incPtr by = do
  ptr <- getPtr
  jumpTo (ptr + by)

shiftBase :: Int -> MachineState m ()
shiftBase by = do
  base <- getBase
  setBase (base + by)

expandMemory :: Ptr -> MachineState m ()
expandMemory ptr = do
  memory <- getMemory
  let lenMemory = length memory
  if (ptr >= lenMemory) 
  then do
    let newMemory = memory V.++ V.replicate (ptr - lenMemory + 1) 0
    setMemory newMemory
  else return ()

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
  expandMemory ptr
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
derefAtPtr RelativeMode ptr = do
  base <- getBase
  reference <- derefAtPtr ImmediateMode ptr
  derefAtPtr ImmediateMode (base + reference)

setAtPtr :: Ptr -> Int -> MachineState m ()
setAtPtr ptr val = do
  expandMemory ptr
  memory <- getMemory
  let newMemory = memory V.// [(ptr, val)]
  setMemory newMemory

setrefAtPtr :: Param -> Ptr -> Int -> MachineState m ()
setrefAtPtr PositionMode ptr val = do
  reference <- derefAtPtr ImmediateMode ptr
  setAtPtr reference val
setrefAtPtr RelativeMode ptr val = do
  base <- getBase
  reference <- derefAtPtr ImmediateMode ptr
  setAtPtr (base + reference) val

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
executeOperation (ShiftBase by)  = do
  ptr <- getPtr
  base <- getBase
  shiftBy <- derefAtPtr by (ptr + 1)
  shiftBase shiftBy
  incPtr 2

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

readInt :: IO Int
readInt = getLine >>= return . read

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
