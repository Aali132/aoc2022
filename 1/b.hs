module Aoc where

import Data.List.Split
import Data.List

parse :: [String] -> [[Integer]]
parse s = (map.map) read (splitOn [""] s)

aoc :: [String] -> String
aoc f = show (sum (take 3 (reverse (sort (map sum (parse f))))))
