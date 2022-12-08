module Aoc where

import Debug.Trace

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

takeWhileOneMore :: (a -> Bool) -> [a] -> [a]
takeWhileOneMore p = foldr (\x ys -> if p x then x:ys else [x]) []

scoreSide :: (Int -> Bool,Int) -> [Int] -> Int -> Int -> Int
scoreSide gen g idx level = (length (takeWhileOneMore (\x -> x > 0) (map (\x -> fromEnum ((g !! x) < level)) (drop 1 (idxGen gen idx)))))

score :: [Int] -> Int -> Int -> Int
score g idx level = product [ scoreSide (gen (side g)) g idx level | gen <- [left,right,top,bottom] ]

parse :: [String] -> [Int]
parse s = concat [ [ read [c] | c <- line ] | line <- s ]

aoc :: [String] -> String
aoc f = show (maximum [ score g idx (g !! idx) | idx <- [0..(length g)-1] ])
  where g = parse f
