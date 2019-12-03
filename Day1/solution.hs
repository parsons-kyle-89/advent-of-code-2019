import System.Environment
import System.Exit

main = do
  fileName <- getArgs >>= parse
  weights <- readWeights fileName
  putStrLn $ show (calculateTotalFuel weights)


parse :: [String] -> IO String
parse [fileName] = return fileName
parse _ = putStrLn "wrong number of arguments" >> exitFailure


readWeights :: String -> IO [Int]
readWeights fileName = do
  fileContents <- readFile fileName
  return $ map read (lines fileContents)


calculateTotalFuel :: [Int] -> Int
calculateTotalFuel weights = sum $ map calculateTyrannyFuel weights


calculateFuel :: Int -> Int
calculateFuel weight = weight `div` 3 - 2


calculateTyrannyFuel :: Int -> Int
calculateTyrannyFuel weight = sum $ tail $ takeWhile (>0) $ iterate calculateFuel weight
