
import System.Environment
import System.Exit
import qualified Data.Map as Map

type Body = (Position, Velocity)
type Position = (Int, Int, Int)
type Velocity = (Int, Int, Int)

main = do
  fileName <- getArgs >>= parseArgs
  contents <- readFile fileName
  let positions = readPositions contents  
  let bodies = initializeBodies positions
  let finalPosition = iterate doStep bodies !! 1000
  let finalEnergy = energy finalPosition
  print $ finalEnergy
  print $ firstRepeatTime bodies

parseArgs :: [String] -> IO String
parseArgs [fileName] = return $ fileName
parseArgs _ = putStrLn "wrong number of arguments" >> exitFailure

readPositions :: String -> [Position]
readPositions contents = map readPosition (lines contents)

readPosition :: String -> Position
readPosition encoding = read $ "(" ++ filter (\c -> not (c `elem` "<>=xyz ")) encoding ++ ")"

initializeBodies :: [Position] -> [Body]
initializeBodies positions = map initializeBody positions

initializeBody :: Position -> Body
initializeBody position = (position, (0, 0, 0))

doStep :: [Body] -> [Body]
doStep bodies = let
  updatedVelocities = updateVelocities bodies in
  updatePositions updatedVelocities

updateVelocities :: [Body] -> [Body]
updateVelocities bodies = map (updateVelocity bodies) bodies

updateVelocity :: [Body] -> Body -> Body
updateVelocity otherBodies body = foldl updatePair body otherBodies

updatePair :: Body -> Body -> Body
updatePair ((x, y, z), (x', y', z')) ((a, b, c), (_, _, _)) = let
  newX' = x' + fromEnum (a `compare` x) - 1
  newY' = y' + fromEnum (b `compare` y) - 1
  newZ' = z' + fromEnum (c `compare` z) - 1 in
  ((x, y, z), (newX', newY', newZ'))

updatePositions :: [Body] -> [Body]
updatePositions bodies = map updatePosition bodies

updatePosition :: Body -> Body
updatePosition ((x, y, z), (x', y', z')) = ((x+x', y+y', z+z'), (x', y', z'))

energy :: [Body] -> Int
energy bodies = sum $ map bodyEnergy bodies

bodyEnergy :: Body -> Int
bodyEnergy (pos, vel) = componentEnergy pos * componentEnergy vel

componentEnergy :: (Int, Int, Int) -> Int
componentEnergy (x, y, z) = abs x + abs y + abs z

firstRepeatTime :: [Body] -> Int
firstRepeatTime bodies = let
  (xRepeat, xPeriod) = repeatPeriod 0 Map.empty $ map xComponents bodies
  (yRepeat, yPeriod) = repeatPeriod 0 Map.empty $ map yComponents bodies
  (zRepeat, zPeriod) = repeatPeriod 0 Map.empty $ map zComponents bodies in
  foldl1 lcm [xPeriod, yPeriod, zPeriod]

repeatPeriod :: Int -> Map.Map [(Int, Int)] Int -> [(Int, Int)] -> (Int, Int)
repeatPeriod step record components =
  if components `Map.member` record
    then let
      previous = record Map.! components in
      (previous, step - previous)
    else let
      newRecord = Map.insert components step record
      updatedVel = updateComponentVelocites components
      updatedPos = updateComponentPositions updatedVel in
      repeatPeriod (step + 1) newRecord updatedPos

updateComponentVelocites :: [(Int, Int)] -> [(Int, Int)]
updateComponentVelocites components = map (updateComponentVelocity components) components

updateComponentVelocity :: [(Int, Int)] -> (Int, Int) -> (Int, Int)
updateComponentVelocity otherComponents component = 
  foldl updatePairComponent component otherComponents

updatePairComponent :: (Int, Int) -> (Int, Int) -> (Int, Int)
updatePairComponent (x, x') (xx, _) =
  case x `compare` xx of
    LT -> (x, x' + 1)
    EQ -> (x, x')
    GT -> (x, x' - 1)

updateComponentPositions :: [(Int, Int)] -> [(Int, Int)]
updateComponentPositions components = map updateComponentPosition components

updateComponentPosition :: (Int, Int) -> (Int, Int)
updateComponentPosition (x, x') = (x + x', x')

xComponents :: Body -> (Int, Int)
xComponents ((x, _, _), (x', _, _)) = (x, x')

yComponents :: Body -> (Int, Int)
yComponents ((_, y, _), (_, y', _)) = (y, y')

zComponents :: Body -> (Int, Int)
zComponents ((_, _, z), (_, _, z')) = (z, z')
