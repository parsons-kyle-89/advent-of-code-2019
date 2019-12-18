import System.Environment
import System.Exit
import Data.List
import Data.Ord (comparing)
import Data.Functor.Identity (Identity (Identity))
import Control.Monad.State (runState)

import Intcode

type PhaseSequence = (Int, Int, Int, Int, Int)

main = do
  fileName <- getArgs >>= parseArgs
  fileContents <- readFile fileName
  let machine = initializeMachine $ read ("[" ++ fileContents ++ "]")
  print $ amplifyMachine machine
  print $ feedbackAmplifyMachine machine

parseArgs :: [String] -> IO String
parseArgs [fileName] = return fileName
parseArgs _ = putStrLn "Wrong number of arguments" >> exitFailure

amplifyMachine :: Machine -> (Int, PhaseSequence)
amplifyMachine machine = maximumBy (comparing fst) (amplifications machine)

amplifications :: Machine -> [(Int, PhaseSequence)]
amplifications machine = do
  phaseSequence <- phaseSequences
  let amplification = amplify phaseSequence 0 machine
  return $ (amplification, phaseSequence)

amplify :: PhaseSequence -> Int -> Machine -> Int
amplify (p1, p2, p3, p4, p5) input machine = let
  Identity (Value output1, _) = machine ==> inputValue p1 >> inputValue input >> getValue
  Identity (Value output2, _) = machine ==> inputValue p2 >> inputValue output1 >> getValue
  Identity (Value output3, _) = machine ==> inputValue p3 >> inputValue output2 >> getValue
  Identity (Value output4, _) = machine ==> inputValue p4 >> inputValue output3 >> getValue
  Identity (Value output5, _) = machine ==> inputValue p5 >> inputValue output4 >> getValue in
  output5

feedbackAmplifyMachine :: Machine -> (Int, PhaseSequence)
feedbackAmplifyMachine machine = 
  maximumBy (comparing fst) (feedbackAmplifications machine)

feedbackAmplifications :: Machine -> [(Int, PhaseSequence)]
feedbackAmplifications machine = do
  phaseSequence <- higherPhaseSequences
  let (ps1, ps2, ps3, ps4, ps5) = initializeAmps machine phaseSequence
  let amplification = feedbackAmplify 0 ps1 ps2 ps3 ps4 ps5
  return $ (amplification, phaseSequence)

feedbackAmplify :: Int -> Machine -> Machine -> Machine 
                    -> Machine -> Machine -> Int
feedbackAmplify input m1 m2 m3 m4 m5 = let
  Identity (output1, newM1) = m1 ==> inputValue input >> getValue in
  case output1 of
    ProgramExit -> input
    Value out1 -> let
      Identity (Value output2, newM2) = m2 ==> inputValue out1 >> getValue
      Identity (Value output3, newM3) = m3 ==> inputValue output2 >> getValue
      Identity (Value output4, newM4) = m4 ==> inputValue output3 >> getValue
      Identity (Value output5, newM5) = m5 ==> inputValue output4 >> getValue in
      feedbackAmplify output5 newM1 newM2 newM3 newM4 newM5

initializeAmps :: Machine -> PhaseSequence -> (Machine, Machine, Machine, Machine, Machine)
initializeAmps machine (p1, p2, p3, p4, p5) = let
  Identity (_, newM1) = machine ==> inputValue p1
  Identity (_, newM2) = machine ==> inputValue p2
  Identity (_, newM3) = machine ==> inputValue p3
  Identity (_, newM4) = machine ==> inputValue p4
  Identity (_, newM5) = machine ==> inputValue p5 in
  (newM1, newM2, newM3, newM4, newM5)

phaseSequences :: [PhaseSequence]
phaseSequences = do
  a <- [0..4]
  b <- [0..4] \\ [a]
  c <- [0..4] \\ [a, b]
  d <- [0..4] \\ [a, b, c]
  e <- [0..4] \\ [a, b, c, d]
  return $ (a, b, c, d, e)

higherPhaseSequences :: [PhaseSequence]
higherPhaseSequences = do
  a <- [5..9]
  b <- [5..9] \\ [a]
  c <- [5..9] \\ [a, b]
  d <- [5..9] \\ [a, b, c]
  e <- [5..9] \\ [a, b, c, d]
  return $ (a, b, c, d, e)
