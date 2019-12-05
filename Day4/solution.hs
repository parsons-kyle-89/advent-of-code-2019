import Data.List (group)


main = print $ solution 6 240298 784956


solution :: Int -> Int -> Int -> Int
solution digits start end = let
  increasing = increasingInts 0 digits
  inRange = inRangeInts start end increasing 
  repeatedDigits = repeatedDigitInts inRange in
  length repeatedDigits


increasingInts :: Int -> Int -> [Int]
increasingInts smallestDigit 1 = [smallestDigit..9]
increasingInts smallestDigit numDigits = 
  concatMap (generateRest numDigits) [smallestDigit..9]


generateRest :: Int -> Int -> [Int]
generateRest numDigits digit = 
  map (+digit*10^(numDigits-1)) (increasingInts digit (numDigits - 1))


inRangeInts :: Int -> Int -> [Int] -> [Int]
inRangeInts start end ints = takeWhile (<=end) $ dropWhile (<start) ints


repeatedDigitInts :: [Int] -> [Int]
repeatedDigitInts ints = filter hasSinglyRepeatedDigits ints


hasRepeatedDigits :: Int -> Bool
hasRepeatedDigits int = let
  chars = show int
  maxGroup = maximum $ map length (group chars) in
  maxGroup >= 2


hasSinglyRepeatedDigits :: Int -> Bool
hasSinglyRepeatedDigits int = let
  chars = show int
  groups = map length (group chars) in
  any (==2) groups
