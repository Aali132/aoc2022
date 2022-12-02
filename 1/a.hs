module Aoc where

import Data.List.Split

parse :: [String] -> [[Integer]]
parse s = (map.map) read (splitOn [""] s)

aoc :: [String] -> String
aoc f = show (maximum (map sum (parse f)))
