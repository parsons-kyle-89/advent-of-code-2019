
import System.Environment
import System.Exit
import qualified Data.Map.Strict as Map
import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import Data.List.Split (splitOn)

type Element = String
data Ingredient = Ingredient { amount :: Int, element :: Element } deriving (Show)
data Formula = Formula { to :: Ingredient, from :: [Ingredient] } deriving (Show)
type Table = Map.Map Element Formula

main = do
  fileName <- getArgs >>= parseArgs
  contents <- readFile fileName
  let conversions = parseContents contents
  print conversions

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
  (amount, element) = break isSpace $ dropWhile isSpace $ dropWhileEnd isSpace ingredient in
  Ingredient (read amount) element

parseFrom :: String -> [Ingredient]
parseFrom from = map parseIngredient (splitOn "," from) 
