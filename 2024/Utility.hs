module Utility where

toNumbers :: String -> [Int]
toNumbers = map read . words