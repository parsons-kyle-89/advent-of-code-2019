module Intcode ( evaluate ) where

import System.Exit (exitSuccess, exitFailure)
import qualified Data.Vector as V

type Progn = V.Vector Int
type Instruction = Int -> Progn -> IO (Progn, Int)

evaluate :: Int -> Progn -> IO Progn
evaluate ptr progn = do
  let instructionCode = progn V.! ptr
  let instruction = getInstruction instructionCode
  (newProgn, step) <- instruction ptr progn
  if instructionCode /= 99
  then evaluate (ptr + step) newProgn
  else return newProgn


getInstruction :: Int -> Instruction
getInstruction 99 = exitInstruction
getInstruction 1 = addInstruction
getInstruction 2 = multiplyInstruction
getInstruction _ = failInstruction

operInstruction :: (Int -> Int -> Int) -> Instruction
operInstruction oper ptr progn = let
  inreg1 = progn V.! (ptr + 1)
  inreg2 = progn V.! (ptr + 2)
  inval1 = progn V.! inreg1
  inval2 = progn V.! inreg2
  outval = inval1 `oper` inval2
  outreg = progn V.! (ptr + 3) in
  return (progn V.// [(outreg, outval)], 4)

addInstruction :: Instruction
addInstruction = operInstruction (+)

multiplyInstruction :: Instruction
multiplyInstruction = operInstruction (*)

exitInstruction ::  Instruction
exitInstruction _ progn = return (progn, 1)

failInstruction :: Instruction
failInstruction _ _ = exitFailure
