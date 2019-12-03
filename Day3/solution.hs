import System.Environment
import System.Exit
import Data.List.Split (splitOn)
import Data.List (minimumBy)
import Data.Ord (comparing)
import qualified Data.Set as Set

data Problem = One deriving (Show)
data Direction = U | D | L | R deriving (Show, Read)
type Position = (Int, Int)


main = do
  (problem, directions) <- parseInput
  let solution = case problem of
                   One -> solutionOne directions in
    putStrLn $ show solution
  

solutionOne :: [[Direction]] -> Int
solutionOne directionss = let
  pointss = map directionsToPoints directionss
  overlapPoints = foldl1 Set.intersection pointss 
  nonOriginOverlapPoints = Set.delete (0,0) overlapPoints in
  Set.findMin $ Set.map l1Norm nonOriginOverlapPoints


directionsToPoints :: [Direction] -> Set.Set Position
directionsToPoints directions = Set.fromList $
  scanl updatePosition (0,0) directions


updatePosition :: Position -> Direction -> Position
updatePosition (x, y) U = (x+1, y)
updatePosition (x, y) D = (x-1, y)
updatePosition (x, y) L = (x, y-1)
updatePosition (x, y) R = (x, y+1)


l1Norm :: Position -> Int
l1Norm (x, y) = abs x + abs y


parseInput :: IO (Problem, [[Direction]])
parseInput = do
  args <- getArgs
  (problem, argsRest) <- popProblem args
  (directions, []) <- popDirections argsRest
  return (problem, directions)
  

popProblem :: [String] -> IO (Problem, [String])
popProblem ("1" : argsRest) = return (One, argsRest)
popProblem _ = putStrLn "problem parse error" >> exitFailure


popDirections :: [String] -> IO ([[Direction]], [String])
popDirections (fileName : argsRest) = do
  fileContents <- readFile fileName
  return (parseDirections fileContents, argsRest)


parseDirections :: String -> [[Direction]]
parseDirections fileContents = map ((concatMap parseSegment) . commaSplit)
                                   (lines fileContents)


commaSplit :: String -> [String]
commaSplit string = splitOn [','] string


parseSegment :: String -> [Direction]
parseSegment (d : n) = replicate (read n) (read [d])
