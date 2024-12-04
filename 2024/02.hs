import Data.List (tails)
import System.IO (readFile)
import Utility (toNumbers)

main :: IO ()
main = do
  values <- fmap (map toNumbers . lines) (readFile "data/02.txt")
  let differences = calculateDifferences values
  let part1Answer = length (filter id (map isSafe differences))
  let correctedDifferences = fmap (calculateDifferences . withoutEntries) values
  let part2Answer = length (filter id (map (any isSafe) correctedDifferences))

  putStrLn $ "Answer (Part 1): " ++ show part1Answer
  putStrLn $ "Answer (Part 2): " ++ show part2Answer

calculateDifferences :: [[Int]] -> [[Int]]
calculateDifferences = map (map (\[x, y] -> x - y) . windows 2)

isSafe :: [Int] -> Bool
isSafe xs = all (\x -> abs x >= 1 && abs x <= 3) xs && (all (> 0) xs || all (< 0) xs)

windows :: Int -> [a] -> [[a]]
windows size = takeWhile ((>= size) . length) . map (take size) . tails

withoutEntries :: [a] -> [[a]]
withoutEntries list = [take i list ++ drop (i + 1) list | i <- [0 .. length list - 1]]