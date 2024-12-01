import Data.List (group, lookup, sort)
import Data.Maybe (fromMaybe)
import System.IO (readFile)

main :: IO ()
main = do
  (leftValues, rightValues) <- fmap (splitAlternating . toNumbers) (readFile "data/01.txt")
  let sortedLeftValues = sort leftValues
  let sortedRightValues = sort rightValues
  let part1Answer = sum (zipWith (\left right -> abs (left - right)) sortedLeftValues sortedRightValues)
  let instanceCounts = map (\groupedValues -> (head groupedValues, length groupedValues)) . group $ sortedRightValues
  let part2Answer = sum (map (\value -> value * fromMaybe 0 (lookup value instanceCounts)) sortedLeftValues)

  putStrLn $ "Answer (Part 1): " ++ show part1Answer
  putStrLn $ "Answer (Part 2): " ++ show part2Answer

splitAlternating :: [a] -> ([a], [a])
splitAlternating xs = (evenValues, oddValues)
  where
    evenValues = [value | (value, i) <- zip xs [0 ..], even i]
    oddValues = [value | (value, i) <- zip xs [0 ..], odd i]

toNumbers :: String -> [Int]
toNumbers = map read . words