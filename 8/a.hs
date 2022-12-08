module Aoc where

isqrt :: Int -> Int
isqrt = floor . sqrt . fromIntegral

side :: [Int] -> Int
side g = isqrt (length g)

left side = (\idx -> ((mod idx side) == 0),-1)
right side = (\idx -> ((mod idx side) == (side-1)),1)
top side = (\idx -> (idx < side),-side)
bottom side = (\idx -> (idx >= side * (side-1)),side)

idxGen :: (Int -> Bool,Int) -> Int -> [Int]
idxGen (stopCond,offset) idx
  | stopCond idx = [idx]
  | otherwise = idx:(idxGen (stopCond,offset) (idx+offset))

visibleSide :: (Int -> Bool,Int) -> [Int] -> Int -> Int -> Bool
visibleSide gen g idx level = and (map (\x -> (g !! x) < level) (drop 1 (idxGen gen idx)))

visible :: [Int] -> Int -> Int -> Bool
visible g idx level = or [ visibleSide (gen (side g)) g idx level | gen <- [left,right,top,bottom] ]

parse :: [String] -> [Int]
parse s = concat [ [ read [c] | c <- line ] | line <- s ]

aoc :: [String] -> String
aoc f = show (sum (map fromEnum [ visible g idx (g !! idx) | idx <- [0..(length g)-1] ]))
  where g = parse f
