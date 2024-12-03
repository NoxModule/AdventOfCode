import Data.List (tails)
import System.IO (readFile)
import Utility (toNumbers)

main :: IO ()
main = do
  values <- fmap (map toNumbers . lines) (readFile "data/02.txt")
  let distances = calculateDistances values
  let isSafeRanges = map isSafeDistances distances
  let part1Answer = length (filter id (zipWith (&&) isSafeRanges (map isMonotonic distances)))
  let correctedDistances = map (calculateDistances . withoutEntries) values
  let correctedIsSafeRanges = map (any isSafeDistances) correctedDistances
  let correctedIsMonotonics = map (any isMonotonic) correctedDistances
  let part2Answer = length (filter id (zipWith (&&) correctedIsMonotonics correctedIsSafeRanges))

  putStrLn $ "Answer (Part 1): " ++ show part1Answer
  putStrLn $ "Answer (Part 2): " ++ show part2Answer

calculateDistances :: [[Int]] -> [[Int]]
calculateDistances = map (map (\[a, b] -> a - b) . windows 2)

isMonotonic :: [Int] -> Bool
isMonotonic [] = False
isMonotonic (firstValue : list)
  | firstValue == 0 = False
  | firstValue > 0 = all (> 0) list
  | otherwise = all (< 0) list

isSafeDistances :: [Int] -> Bool
isSafeDistances = all (\x -> abs x >= 1 && abs x <= 3)

windows :: Int -> [a] -> [[a]]
windows size = takeWhile ((>= size) . length) . map (take size) . tails

withoutEntries :: [a] -> [[a]]
withoutEntries list = [take i list ++ drop (i + 1) list | i <- [0 .. length list - 1]]