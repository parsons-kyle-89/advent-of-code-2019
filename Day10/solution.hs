import System.Environment (getArgs)
import System.Exit

import Data.List
import Data.Ord (comparing)
import Data.Fixed (mod')

type Asteroid = (Int, Int)
type Displacement = (Int, Int)
type Bounds = (Int, Int, Int, Int)

getX = fst
getY = snd

main = do
  asteroidsEncoding <- getArgs >>= parseArgs >>= readFile
  let asteroids = readAsteroids asteroidsEncoding
  let (sta, numAsteroids) = bestStation asteroids
  print $ (sta, numAsteroids)
  let otherAsteroids = delete sta asteroids
  let vaporized = vaporizationOrder sta otherAsteroids
  print $ vaporized !! 199

parseArgs :: [String] -> IO String
parseArgs [fileName] = return fileName
parseArgs _ = putStrLn "Wrong number of arguments" >> exitFailure

readAsteroids :: String -> [Asteroid]
readAsteroids encoding = concatMap (uncurry readRow) (zip (lines encoding) [0..])

readRow :: String -> Int -> [Asteroid]
readRow rowEncoding rowNum = [(colNum, rowNum) | colNum <- findIndices (=='#') rowEncoding]

vaporizationOrder :: Asteroid -> [Asteroid] -> [Asteroid]
vaporizationOrder _ [] = []
vaporizationOrder sta asteroids = let
  thisRound = visibleFrom sta asteroids
  thisRoundOrdered = sortBy (comparing (clockwiseFrom sta)) thisRound in
  thisRoundOrdered ++ vaporizationOrder sta (asteroids \\ thisRound)

bestStation :: [Asteroid] -> (Asteroid, Int)
bestStation asteroids = let
  numVisibleFrom = (\a -> length (visibleFrom a asteroids))
  bestSta = maximumBy (comparing numVisibleFrom) asteroids in
  (bestSta, numVisibleFrom bestSta)

visibleFrom :: Asteroid -> [Asteroid] -> [Asteroid]
visibleFrom sta asteroids = let
  bounds = getBounds asteroids
  otherAsteroids = delete sta asteroids
  blocked = nub (concatMap (blockedFrom bounds sta) otherAsteroids) in
  otherAsteroids \\ blocked

blockedFrom :: Bounds -> Asteroid -> Asteroid -> [Asteroid]
blockedFrom bounds sta asteroid = let
  displacement = asteroid .- sta 
  (reducedDisplacement, factor) = reduceDisplacement displacement in
  takeWhile (withinBounds bounds) (map ((sta .+) . (reducedDisplacement .*)) 
                                       [(factor + 1)..])

getBounds :: [Asteroid] -> Bounds
getBounds asteroids = let
  xMin = minimum (map getX asteroids)
  xMax = maximum (map getX asteroids)
  yMin = minimum (map getY asteroids)
  yMax = maximum (map getY asteroids) in
  (xMin, xMax, yMin, yMax)

withinBounds :: Bounds -> Asteroid -> Bool
withinBounds (xMin, xMax, yMin, yMax) (x, y) =
  (xMin <= x) && (x <= xMax) && (yMin <= y) && (y <= yMax)

(.+) :: Asteroid -> Displacement -> Asteroid
(.+) (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

(.-) :: Asteroid -> Asteroid -> Displacement
(.-) (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)

(.*) :: Displacement -> Int-> Displacement
(.*) (x, y) k = (x * k, y * k)

reduceDisplacement :: Displacement -> (Displacement, Int)
reduceDisplacement (x, y) = let
  factor = gcd x y in
  ((x `div` factor, y `div` factor), factor)

clockwiseFrom :: Asteroid -> Asteroid -> Float
clockwiseFrom sta asteroid = let
  (x, y) = asteroid .- sta in
  (atan2 (fromIntegral x) (fromIntegral (-y))) `mod'` (2 * pi)
