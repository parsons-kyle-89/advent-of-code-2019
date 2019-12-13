import System.Environment
import System.Exit
import Data.List
import Data.Ord (comparing)
import Control.Monad.State (runState)

import Intcode

type PhaseSequence = (Int, Int, Int, Int, Int)

main = do
  fileName <- getArgs >>= parseArgs
  prognState <- readPrognState fileName
  print $ amplifyPrognState prognState
  print $ feedbackAmplifyPrognState prognState

parseArgs :: [String] -> IO String
parseArgs [fileName] = return fileName
parseArgs _ = putStrLn "Wrong number of arguments" >> exitFailure

amplifyPrognState :: PrognState -> (Int, PhaseSequence)
amplifyPrognState prognState = maximumBy (comparing fst) (amplifications prognState)

amplifications :: PrognState -> [(Int, PhaseSequence)]
amplifications prognState = do
  phaseSequence <- phaseSequences
  let amplification = amplify phaseSequence 0 prognState
  return $ (amplification, phaseSequence)

amplify :: PhaseSequence -> Int -> PrognState -> Int
amplify (p1, p2, p3, p4, p5) input prognState = let
  (_, (_, output1)) = runState (executePrognListIO prognState) ([p1, input], [])
  (_, (_, output2)) = runState (executePrognListIO prognState) (p2:output1, [])
  (_, (_, output3)) = runState (executePrognListIO prognState) (p3:output2, [])
  (_, (_, output4)) = runState (executePrognListIO prognState) (p4:output3, [])
  (_, (_, output5)) = runState (executePrognListIO prognState) (p5:output4, []) in
  head output5

feedbackAmplifyPrognState :: PrognState -> (Int, PhaseSequence)
feedbackAmplifyPrognState prognState = 
  maximumBy (comparing fst) (feedbackAmplifications prognState)

feedbackAmplifications :: PrognState -> [(Int, PhaseSequence)]
feedbackAmplifications ps = do
  phaseSequence <- higherPhaseSequences
  let (ps1, ps2, ps3, ps4, ps5) = initializeAmps ps phaseSequence
  let amplification = feedbackAmplify 0 ps1 ps2 ps3 ps4 ps5
  return $ (amplification, phaseSequence)

feedbackAmplify :: Int -> PrognState -> PrognState -> PrognState 
                    -> PrognState -> PrognState -> Int
feedbackAmplify input ps1 ps2 ps3 ps4 ps5 = let
  ops@[op1, op2, op3, op4, op5] = map semanticAnalysis [ps1, ps2, ps3, ps4, ps5] in
  if any isExit ops
  then input
  else let 
    (newps1, output1) = produceResult op1 ps1 ([input], [])
    (newps2, output2) = produceResult op2 ps2 ([output1], [])
    (newps3, output3) = produceResult op3 ps3 ([output2], [])
    (newps4, output4) = produceResult op5 ps4 ([output3], [])
    (newps5, output5) = produceResult op5 ps5 ([output4], []) in
    feedbackAmplify output5 newps1 newps2 newps3 newps4 newps5

produceResult :: Operation -> PrognState -> ([Int], [Int]) -> (PrognState, Int)
produceResult _ prognState (_, [output]) = (prognState, output)
produceResult oper prognState listState = let
  (newPrognState, newListState) = runState (executeOperListIO oper prognState) listState
  newOper = semanticAnalysis newPrognState in
  produceResult newOper newPrognState newListState

initializeAmps :: PrognState -> PhaseSequence 
                  -> (PrognState, PrognState, PrognState, PrognState, PrognState)
initializeAmps ps (p1, p2, p3, p4, p5) = let
  newps1 = initializeAmp ps ([p1], [])
  newps2 = initializeAmp ps ([p2], [])
  newps3 = initializeAmp ps ([p3], [])
  newps4 = initializeAmp ps ([p4], [])
  newps5 = initializeAmp ps ([p5], []) in
  (newps1, newps2, newps3, newps4, newps5)

initializeAmp :: PrognState -> ([Int], [Int]) -> PrognState
initializeAmp prognState ([], _) = prognState
initializeAmp prognState listState = let
  oper = semanticAnalysis prognState
  (newPrognState, newListState) = runState (executeOperListIO oper prognState) listState in
  initializeAmp newPrognState newListState

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
