
import System.Environment
import System.Exit
import qualified Data.Map.Strict as Map
import Data.Char (isSpace)
import Data.Function (on)
import Data.Ord (comparing)
import Data.List (dropWhileEnd, sortBy, groupBy)
import Data.List.Split (splitOn)

type Element = String
data Ingredient = Ingredient { amount :: Int, element :: Element } deriving (Eq, Show)
data Formula = Formula { to :: Ingredient, from :: [Ingredient] } deriving (Show)
type Table = Map.Map Element Formula

main = do
  fileName <- getArgs >>= parseArgs
  contents <- readFile fileName
  let rawConversions = parseContents contents
  let conversions = Map.insert "ORE" (Formula (Ingredient 1 "ORE") [(Ingredient 1 "ORE")]) rawConversions
  print $ transformAll conversions [(Ingredient 1 "FUEL")]

parseArgs :: [String] -> IO String
parseArgs [fileName] = return $ fileName
parseArgs _ = putStrLn "wrong number of arguments" >> exitFailure

parseContents :: String -> Table
parseContents contents = Map.fromList (map parseLine (lines contents))

parseLine :: String -> (Element, Formula)
parseLine line = let
  (from, to) = splitLine line
  fromIngredients = parseFrom from
  toIngredient@(Ingredient _ toElement) = parseIngredient to in
  (toElement, Formula toIngredient fromIngredients)

splitLine :: String -> (String, String)
splitLine line = let
  (from, '=':'>':to) = break (== '=') line in
  (from, to)

parseIngredient :: String -> Ingredient
parseIngredient ingredient = let
  (amount, ' ':element) = break isSpace $ dropWhile isSpace $ dropWhileEnd isSpace ingredient in
  Ingredient (read amount) element

parseFrom :: String -> [Ingredient]
parseFrom from = map parseIngredient (splitOn "," from) 

transformAll :: Table -> [Ingredient] -> [Ingredient]
transformAll conversions froms = let
  transformer = simplify . (transformOnce conversions) in
  until (idempotent transformer) transformer froms

transformOnce :: Table -> [Ingredient] -> [Ingredient]
transformOnce conversions froms = concatMap (transform conversions) froms

transform :: Table -> Ingredient -> [Ingredient]
transform conversions (Ingredient neededAmount neededElement) = let
  Just (Formula (Ingredient producedAmount _) ingredients) = Map.lookup neededElement conversions
  numberReactions = neededAmount `divUp` producedAmount 
  leftOverNeeded = numberReactions * producedAmount - neededAmount
  fromIngredients = multiplyIngredients numberReactions ingredients in
  if leftOverNeeded == 0
  then fromIngredients
  else (Ingredient leftOverNeeded neededElement) : fromIngredients

multiplyIngredients :: Int -> [Ingredient] -> [Ingredient]
multiplyIngredients factor ingredients = map (multiplyIngredient factor) ingredients

multiplyIngredient :: Int -> Ingredient -> Ingredient
multiplyIngredient factor (Ingredient amount element) = Ingredient (factor * amount) element

idempotent :: (Eq a) => (a -> a) -> a -> Bool
idempotent f = \x -> x == f x

divUp :: Int -> Int -> Int
divUp num denom = -((-num) `div` denom)

simplify :: [Ingredient] -> [Ingredient]
simplify ingredients = map sumIngredient (groupIngredients ingredients)

sumIngredient :: [Ingredient] -> Ingredient
sumIngredient ingredients = foldl1 (\(Ingredient a e) (Ingredient b _) -> (Ingredient (a + b) e)) ingredients

groupIngredients :: [Ingredient] -> [[Ingredient]]
groupIngredients ingredients = groupBy ((==) `on` element) $ sortBy (comparing element) ingredients
