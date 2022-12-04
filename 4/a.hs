module Aoc where

import Data.List.Split

overlap :: (Integer,Integer,Integer,Integer) -> Integer
overlap (s1,e1,s2,e2)
  | s1 >= s2 && e1 <= e2 = 1
  | s2 >= s1 && e2 <= e1 = 1
  | otherwise = 0

tuple4 :: [Integer] -> (Integer,Integer,Integer,Integer)
tuple4 (a:b:c:d:[]) = (a,b,c,d)

parseLine :: String -> (Integer,Integer,Integer,Integer)
parseLine l = tuple4 (map read (concat (map (splitOn "-") (splitOn "," l))))

parse :: [String] -> [(Integer,Integer,Integer,Integer)]
parse s = map parseLine s

aoc :: [String] -> String
aoc f = show (sum (map overlap (parse f)))
