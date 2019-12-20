{-# language RankNTypes, FlexibleContexts #-}

import System.Environment
import System.Exit
import Data.List.Split (chunksOf)
import Control.Monad.Trans.State

import Intcode

type MachineState m a = (Monad m) => StateT Machine m a
data Datum =   Datum { _x :: Int, _y :: Int, _pixel :: Pixel } 
             | Score { _score :: Int } deriving (Show)
data Pixel = Empty | Wall | Block | Paddle | Ball deriving (Show, Enum, Eq)

isPixelDatum :: Datum -> Bool
isPixelDatum (Datum _ _ _) = True
isPixelDatum _ = False

main = do
  fileName <- getArgs >>= parseArgs
  fileContents <- readFile fileName
  let progn = read ("[" ++ fileContents ++ "]")
  (values, _) <- initializeMachine progn ==> getAllValues
  print $ gameStats values

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

