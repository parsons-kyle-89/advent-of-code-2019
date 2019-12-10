import System.Environment (getArgs)
import System.Exit
import Data.Char (digitToInt)
import Data.List (intercalate)
import Data.List.Split (chunksOf)
import Data.Foldable (minimumBy)
import Data.Ord (comparing)

type Image = [Layer]
type Layer = [Row]
type Row = [Pixel]
type Pixel = Int

main = do
  (width, height, encoding) <- getArgs >>= parseArgs
  let image = decodeImage width height encoding
  let checksumLayer = minimumBy (comparing (countInLayer 0)) image
  let onesInLayer = countInLayer 1 checksumLayer
  let twosInLayer = countInLayer 2 checksumLayer
  print $ onesInLayer * twosInLayer
  putStrLn $ renderLayer $ flattenImage image

parseArgs :: [String] -> IO (Int, Int, String)
parseArgs [widthArg, heightArg, fileName] = do
  let width = read widthArg
  let height = read heightArg
  rawEncoding <- readFile fileName
  let encoding = init rawEncoding
  return (width, height, encoding)
parseArgs _ = putStrLn "Wrong number of arguments" >> exitFailure

decodeImage :: Int -> Int -> String -> Image
decodeImage width height encoding = let
  rawData = map digitToInt encoding in
  map (decodeLayer width height) (chunksOf (width * height) rawData)

decodeLayer :: Int -> Int -> [Int] -> Layer
decodeLayer width height rawData = map (decodeRow width) (chunksOf width rawData)

decodeRow :: Int -> [Int] -> Row
decodeRow width rawData = rawData

countInLayer :: Int -> Layer -> Int
countInLayer number layer = sum $ map (countInRow number) layer

countInRow :: Int -> Row -> Int
countInRow number = length . filter (==number)

flattenImage :: Image -> Layer
flattenImage image = foldl1 flattenLayers image

flattenLayers :: Layer -> Layer -> Layer
flattenLayers top bottom = map (uncurry flattenRows) (zip top bottom)

flattenRows :: Row -> Row -> Row
flattenRows top bottom = map (uncurry flattenPixels) (zip top bottom)

flattenPixels :: Pixel -> Pixel -> Pixel
flattenPixels 2 bottom = bottom
flattenPixels top _ = top

renderLayer :: Layer -> String
renderLayer layer = intercalate "\n" $ map renderRow layer

renderRow :: Row -> String
renderRow row = map renderChar row

renderChar :: Pixel -> Char
renderChar 0 = '█'
renderChar 1 = '░'
renderChar 2 = '▓'
