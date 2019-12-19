{-# language RankNTypes, FlexibleContexts #-}

import System.Environment (getArgs)
import System.Exit
import qualified Data.Map as Map
import Data.Functor.Identity (Identity (Identity))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State

import Intcode

type PainterState m a = (Monad m) => StateT HullPainter (StateT Machine m) a
data HullPainter = HullPainter Hull Location Direction Int deriving (Show)
type Hull = Map.Map Location HullSection
data HullSection = Black | White deriving (Show, Enum)
type Location = (Int, Int)
data Direction = North | East | South | West deriving (Show, Enum)

main = do
  fileName <- getArgs >>= parseArgs
  fileContents <- readFile fileName
  let progn = read ("[" ++ fileContents ++ "]")
  ((_, HullPainter _ _ _ work), _) <- initializeMachine progn ==>
                                        initializeHullPainter progn ==>
                                          runHullPainter
  print $ work

parseArgs :: [String] -> IO String
parseArgs [fileName] = return fileName
parseArgs _ = putStrLn "Wrong number of arguments" >> exitFailure

initializeHullPainter :: [Int] -> HullPainter
initializeHullPainter memory = let
  hull = Map.empty :: Hull
  location = (0, 0)
  direction = North
  totalWork = 0 in
  HullPainter hull location direction totalWork

runHullPainter :: PainterState m ()
runHullPainter = do
  isExiting' <- lift $ isExiting
  if isExiting' 
  then return ()
  else do
    inputHullSection
    paint
    turn
    step
    runHullPainter

inputHullSection :: PainterState m ()
inputHullSection = do
  hullSection <- getCurrentHullSection
  lift $ inputValue (hullSectionEncoding hullSection)

paint :: PainterState m ()
paint = do
  intcodeResponse <- lift $ getValue
  case intcodeResponse of
    Value colorToPaintEncoding -> do
      let colorToPaint = hullSectionDecoding colorToPaintEncoding
      location <- getLocation
      everPainted <- wasEverPainted location
      if not everPainted
        then incWork
        else return ()
      paintLocation colorToPaint location
    otherwise -> return ()

wasEverPainted :: Location -> PainterState m Bool
wasEverPainted location = do
  hull <- getHull
  return $ location `Map.member` hull

data Turn = CCW | CW deriving (Enum)

turn :: PainterState m ()
turn = do
  intcodeResponse <- lift $ getValue
  case intcodeResponse of
    Value directionToTurnEncoding -> do
      let directionToTurn = turnDecoding directionToTurnEncoding
      turnDirection directionToTurn
    otherwise -> return ()

step :: PainterState m ()
step = do
  location <- getLocation
  direction <- getDirection
  let newLocation = location `directionAdd` direction
  setLocation newLocation

turnDirection :: Turn -> PainterState m ()
turnDirection turn = do
  direction <- getDirection
  let newDirection = direction `turnAdd` turn
  setDirection newDirection

paintLocation :: HullSection -> Location -> PainterState m ()
paintLocation colorToPaint location = do
  hull <- getHull
  let newHull = Map.insert location colorToPaint hull
  setHull newHull

directionAdd :: Location -> Direction -> Location
directionAdd (x, y) North = (x, y + 1)
directionAdd (x, y) East = (x + 1, y)
directionAdd (x, y) South = (x, y - 1)
directionAdd (x, y) West = (x - 1, y)

turnAdd :: Direction -> Turn -> Direction
turnAdd dir turn = toEnum (((fromEnum dir) + (toOffset turn)) `mod` 4)

toOffset :: Turn -> Int
toOffset CCW = (-1)
toOffset CW = 1

turnDecoding :: Int -> Turn
turnDecoding = toEnum

hullSectionEncoding :: HullSection -> Int
hullSectionEncoding = fromEnum

hullSectionDecoding :: Int -> HullSection
hullSectionDecoding = toEnum

getCurrentHullSection :: PainterState m HullSection
getCurrentHullSection = do
  location <- getLocation
  getHullSectionAtLocation location

getHullSectionAtLocation :: Location -> PainterState m HullSection
getHullSectionAtLocation location = do
  hull <- getHull
  return $ Map.findWithDefault Black location hull

incWork :: PainterState m ()
incWork = do
  work <- getWork
  setWork (work + 1)

getHull :: PainterState m Hull
getHull = state $ \hp@(HullPainter hull _ _ _) -> (hull, hp)

getLocation :: PainterState m Location
getLocation = state $ \hp@(HullPainter _ location _ _) -> (location, hp)

getDirection :: PainterState m Direction
getDirection = state $ \hp@(HullPainter _ _ direction _) -> (direction, hp)

getWork :: PainterState m Int
getWork = state $ \hp@(HullPainter _ _ _ work) -> (work, hp)

setHull :: Hull -> PainterState m ()
setHull newHull = state $ \(HullPainter _ l d w) -> ((), HullPainter newHull l d w)

setLocation :: Location -> PainterState m ()
setLocation newLocation = state $ \(HullPainter h _ d w) -> ((), HullPainter h newLocation d w)

setDirection :: Direction -> PainterState m ()
setDirection newDirection = state $ \(HullPainter h l _ w) -> ((), HullPainter h l newDirection w)

setWork :: Int -> PainterState m ()
setWork newWork = state $ \(HullPainter h l d _) -> ((), HullPainter h l d newWork)
