module Aoc where

startOfPacket :: String -> Integer -> Integer
startOfPacket (a:b:c:d:s) i
  | a/=b, a/=c, a/=d, b/=c, b/=d, c/=d = i
  | otherwise = startOfPacket (b:c:d:s) i+1

aoc :: [String] -> String
aoc f = show (startOfPacket (head f) 4)
