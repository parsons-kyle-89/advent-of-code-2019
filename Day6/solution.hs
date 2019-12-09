import Data.List.Split (splitOn)
import Data.List (partition)

main = do
  print $ totalRelations emptyTree
  print $ totalRelations tree1
  print $ totalRelations tree2
  print $ totalRelations tree3
  print $ decodeOrbitRels orbitRelEncoding

data Tree = Node [Tree] deriving (Show)
type Body = String
type OrbitRel = (Body, Body)

orbited :: OrbitRel -> Body
orbited = fst

orbits :: OrbitRel -> Body
orbits = snd

totalRelations :: Tree -> Int
totalRelations (Node trees) =
  length trees +
  sum (fmap localRelations trees) +
  sum (fmap totalRelations trees)
  where
    localRelations (Node trees) = length trees + sum (fmap localRelations trees)

decodeOrbitRels :: String -> [OrbitRel]
decodeOrbitRels encoding = fmap decodeOrbitRel (lines encoding)

decodeOrbitRel :: String -> OrbitRel
decodeOrbitRel encoding  = let orbited : orbits : [] = splitOn ")" encoding in (orbited, orbits)

treeFromOrbitRels :: Body -> [OrbitRel] -> Tree
treeFromOrbitRels root orbitRels = let
  localOrbitRels = filter ((==root).orbited) orbitRels 
  localOrbits = fmap orbits localOrbitRels in
  Node $ fmap (\newRoot -> treeFromOrbitRels newRoot orbitRels) localOrbits

emptyTree :: Tree
emptyTree = Node []
{-
*
1
-}

tree1 :: Tree
tree1 = Node [Node [], Node []]
{-  

  *
 /
*
 \
  *

2
-}

tree2 :: Tree
tree2 = Node [Node [Node [Node [Node [Node []]]]]]
{-

* - * - * - * - * - *

15
-}

tree3 :: Tree
tree3 = Node [Node [], Node[], Node [Node [], Node [], Node [Node [Node []]]]]
{-  

  *   *
 /   /
* - * - * - * - *
 \   \
  *   *

16
-}

orbitRelEncoding :: String
orbitRelEncoding = "COM)A\nCOM)B\nA)C"
