{-# language RankNTypes, FlexibleContexts #-}

import System.Environment
import System.Exit
import System.IO
import Control.Concurrent (threadDelay)
import Data.List (partition, sortBy)
import Data.List.Split (chunksOf)
import Data.Ord (comparing)
import qualified Data.Map as Map
import Control.Monad.Trans.State
import Control.Monad.Trans.Class (lift)

import Intcode

type ScreenState m a = (Monad m) => StateT Screen (StateT Machine m) a
type MachineState m a = (Monad m) => StateT Machine m a
data Datum =   Datum { _x :: Int, _y :: Int, _pixel :: Pixel } 
             | Score { _score :: Int } deriving (Show)
data Pixel = Empty | Wall | Block | Paddle | Ball deriving (Enum, Eq)
type Location = (Int, Int)
data Screen = Screen {_pixels :: Map.Map Location Pixel, 
                      _screenScore :: Int, 
                      _xMax :: Int,
                      _yMax :: Int}

instance Show Pixel where
  show Empty = " "
  show Wall = "|"
  show Block = "x"
  show Paddle = "-"
  show Ball = "o"

instance Show Screen where
  show (Screen pixels score xMax yMax) = unlines (map (showRow pixels) [0..yMax]) ++ "\n" ++ show score

showRow :: Map.Map Location Pixel -> Int -> String
showRow pixels row = let
  thisRow = getRow pixels row
  fatRow = fattenRow thisRow in
  showFatRow fatRow

getRow :: Map.Map Location Pixel -> Int -> [(Int, Pixel)]
getRow pixels row = [(x, pixel) | ((x, y), pixel) <- Map.toList pixels, y == row]

fattenRow :: [(Int, Pixel)] -> [(Int, Pixel)]
fattenRow [] = []
fattenRow row = let
  xMax = maximum (map fst row)
  defaultRow = Map.fromList [(x, Empty) | x <- [0..xMax]] in
  Map.toList (Map.union (Map.fromList row) defaultRow)

showFatRow :: [(Int, Pixel)] -> String
showFatRow fatRow = let
  sortedRow = sortBy (comparing fst) fatRow in
  concat $ map (show . snd) sortedRow

isPixelDatum :: Datum -> Bool
isPixelDatum (Datum _ _ _) = True
isPixelDatum _ = False

main = do
  fileName <- getArgs >>= parseArgs
  fileContents <- readFile fileName
  let progn = read ("[" ++ fileContents ++ "]")
  (values, _) <- initializeMachine progn ==> getAllValues
  let stats@(xMax, yMax, _) = gameStats values
  print $ stats
  let cheatedProgn = addQuarters progn
  hSetBuffering stdin NoBuffering
  initializeMachine cheatedProgn ==> initializeScreen xMax yMax ==> runWithScreen

parseArgs :: [String] -> IO String
parseArgs [fileName] = return fileName
parseArgs _ = putStrLn "Wrong number of arguments" >> exitFailure

gameStats :: [Int] -> (Int, Int, Int)
gameStats values = let
  data_ = map parseDatum (chunksOf 3 values)
  pixelData = filter isPixelDatum data_
  maxX = maximum (map _x pixelData)
  maxY = maximum (map _y pixelData)
  blocks = length (filter (\(Datum _ _ p) -> p == Block) pixelData) in
  (maxX, maxY, blocks)

parseDatum :: [Int] -> Datum
parseDatum ((-1):0:s:[]) = Score s
parseDatum (x:y:t:[]) = Datum x y (toEnum t)

initializeScreen :: Int -> Int -> Screen
initializeScreen xMax yMax = Screen Map.empty 0 xMax yMax

addQuarters :: [Int] -> [Int]
addQuarters (x:xs) = 2:xs

runWithScreen :: ScreenState IO ()
runWithScreen = do
  updateScreen
  currentScreen <- get
  lift $ lift $ print currentScreen
  lift $ lift $ hFlush stdout
  lift $ lift $ threadDelay 3000
--  input <- lift $ lift $ getInput
  input <- autoInput 
  lift $ inputValue input
  runWithScreen

updateScreen :: ScreenState IO ()
updateScreen = do
  screenDiff <- lift getScreenDiff
  currentScreen <- get
  let newScreen = applyScreenDiff screenDiff currentScreen
  put newScreen

getScreenDiff :: MachineState m [Datum]
getScreenDiff = do
  rawData <- getAllValues
  let data_ = map parseDatum (chunksOf 3 rawData)
  return data_

applyScreenDiff :: [Datum] -> Screen -> Screen
applyScreenDiff data_ screen = let
  (pixels, scores) = partition isPixelDatum data_ in
  applyScoreDiff scores (applyPixelDiff pixels screen)

applyScoreDiff :: [Datum] -> Screen -> Screen
applyScoreDiff [] screen = screen
applyScoreDiff ((Score score):scores) (Screen pixels _ xMax yMax) = let
  newScreen = Screen pixels score xMax yMax in
  applyScoreDiff scores newScreen

applyPixelDiff :: [Datum] -> Screen -> Screen
applyPixelDiff data_ (Screen pixels score xMax yMax) = let
  pixelDiff = Map.fromList (map parsePixel data_)
  newPixels = Map.union pixelDiff pixels in
  Screen newPixels score xMax yMax

parsePixel :: Datum -> (Location, Pixel)
parsePixel (Datum x y pixel) = ((x, y), pixel)

autoInput :: ScreenState m Int
autoInput = do
  ballX <- getBallX
  paddleX <- getPaddleX
  return $ autoCommand ballX paddleX

getBallX :: ScreenState m Int
getBallX = do
  currentScreen <- get
  return $ ballX currentScreen

ballX :: Screen -> Int
ballX (Screen pixels _ _ _) = let
  [((x, _), Ball)] = Map.toList $ Map.filter (\p -> case p of; Ball -> True; otherwise -> False) pixels in
  x

getPaddleX :: ScreenState m Int
getPaddleX = do
  currentScreen <- get
  return $ paddleX currentScreen

paddleX :: Screen -> Int
paddleX (Screen pixels _ _ _) = let
  [((x, _), Paddle)] = Map.toList $ Map.filter (\p -> case p of; Paddle -> True; otherwise -> False) pixels in
  x

autoCommand :: Int -> Int -> Int
autoCommand ballX paddleX = case ballX `compare` paddleX of
  LT -> (-1)
  EQ -> 0
  GT -> 1

getInput :: IO Int
getInput = do
  rawInput <- getChar
  case rawInput of
    'z' -> return (-1)
    'x' -> return 0
    ' ' -> return 0
    'c' -> return 1
    otherwise -> getInput
