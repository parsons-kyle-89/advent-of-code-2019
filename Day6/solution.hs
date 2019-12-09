import System.Environment (getArgs)
import System.Exit
import Data.List.Split (splitOn)
import Data.List (partition, (\\))

main = do
  encoding <- getArgs >>= parseArgs >>= readFile
  let orbitRels = decodeOrbitRels encoding
  let orbitDiag = orbitDiagFromOrbitRels "COM" orbitRels
  print $ indirectOrbits orbitDiag
  let [pathToSanta] = pathBetween orbitDiag "YOU" "SAN"
  print $ length pathToSanta - 3

parseArgs :: [String] -> IO String
parseArgs [fileName] = return fileName
parseArgs _ = putStrLn "Wrong number of arguments" >> exitFailure

data OrbitDiag = Node Body [OrbitDiag] deriving (Show)
type Body = String
type OrbitRel = (Body, Body)

orbited :: OrbitRel -> Body
orbited = fst

orbits :: OrbitRel -> Body
orbits = snd

indirectOrbits :: OrbitDiag -> Int
indirectOrbits (Node _ outerOrbits) =
  length outerOrbits +
  sum (fmap directindirectOrbits outerOrbits) +
  sum (fmap indirectOrbits outerOrbits)
  where
    directindirectOrbits (Node _ outerOrbits) = length outerOrbits + sum (fmap directindirectOrbits outerOrbits)

decodeOrbitRels :: String -> [OrbitRel]
decodeOrbitRels encoding = fmap decodeOrbitRel (lines encoding)

decodeOrbitRel :: String -> OrbitRel
decodeOrbitRel encoding  = let orbited : orbits : [] = splitOn ")" encoding in (orbited, orbits)

orbitDiagFromOrbitRels :: Body -> [OrbitRel] -> OrbitDiag
orbitDiagFromOrbitRels com orbitRels = let
  localOrbitRels = filter ((==com).orbited) orbitRels 
  localOrbits = fmap orbits localOrbitRels in
  Node com $ fmap (\newcom -> orbitDiagFromOrbitRels newcom orbitRels) localOrbits

pathTo :: OrbitDiag -> Body -> [[Body]]
pathTo (Node name outerOrbits) target
  | name == target = return $ [name]
  | otherwise = do
      restOfPath <- outerOrbits >>= (\outerOrbit -> pathTo outerOrbit target)
      return $ name : restOfPath

pathBetween :: OrbitDiag -> Body -> Body -> [[Body]]
pathBetween orbitDiag source target = do
  rootToSource <- pathTo orbitDiag source
  rootToTarget <- pathTo orbitDiag target
  let sourceSegment = rootToSource \\ rootToTarget
  let targetSegment = rootToTarget \\ rootToSource
  let intersection = fst $ last $ takeWhile (uncurry (==)) (zip rootToSource rootToTarget)
  return $ (reverse sourceSegment) ++ [intersection] ++ targetSegment

emptyOrbitDiag :: OrbitDiag
emptyOrbitDiag = Node "A" []
{-
A
1
-}

orbitDiag1 :: OrbitDiag
orbitDiag1 = Node "A" [Node "B" [], Node "C" []]
{-  

  B
 /
A
 \
  C

2
-}

orbitDiag2 :: OrbitDiag
orbitDiag2 = Node "A" [Node "B" [Node "C" [Node "D" [Node "E" [Node "F" []]]]]]
{-

A - B - C - D - E - F

15
-}

orbitDiag3 :: OrbitDiag
orbitDiag3 = Node "A" [Node "B" [], Node "D" [], Node "C" [Node "E" [], Node "G" [], Node "F" [Node "H" [Node "I" []]]]]
{-  

  B   E
 /   /
A - C - F - H - I
 \   \
  D   G

16
-}

orbitRelEncoding :: String
orbitRelEncoding = "COM)A\nCOM)B\nA)C"
