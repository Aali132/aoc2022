module Aoc where

import Data.List.Unique

startOfPacket :: String -> Integer -> Integer
startOfPacket (a:s) i
  | allUnique (take 14 (a:s)) = i
  | otherwise = startOfPacket s i+1

aoc :: [String] -> String
aoc f = show (startOfPacket (head f) 14)
