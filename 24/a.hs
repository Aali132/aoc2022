module Aoc where

import Data.List

type Tile = ([Int])
type Map = [[Tile]]

type Pos = (Int,Int)

cmod :: Int -> Int -> Int
cmod a b
  | m < 0 = m + b
  | otherwise = m
  where m = mod a b

width m = length (m !! 0)
height m = length m

getMap :: Map -> Pos -> [Int]
getMap m (x,y) = (m !! y) !! x

expand :: Map -> Int -> Pos -> [Pos]
expand m t (x0,y0) = [ p | (x1,y1) <- [(1,0),(-1,0),(0,1),(0,-1),(0,0)], x0+x1 >= 0, x0+x1 < (width m), y0+y1 >= 0, y0+y1 < (height m), let p = (x0+x1,y0+y1), (getMap m p) !! t == 0 ]

bfs :: Map -> (Int, [Pos]) -> (Int, [Pos])
bfs m (t,ps) = (t+1,nub (concat (map (expand m t) ps)))

checkTile :: [String] -> Int -> Int -> Char -> Int
checkTile m x y c
  | (m !! y) !! x == c = 1
  | otherwise = 0

parseTile :: [String] -> Int -> Int -> Tile
parseTile s x0 y0 = zipWith (+) (zipWith (+) (zipWith (+) left right) up) down
  where left = cycle [ checkTile s (cmod (x0 + x1) (width s)) y0 '<' | x1 <- [0..(width s)-1] ]
        right = cycle [ checkTile s (cmod (x0 - x1) (width s)) y0 '>' | x1 <- [0..(width s)-1] ]
        up = cycle [ checkTile s x0 (cmod (y0 + y1) (height s)) '^' | y1 <- [0..(height s)-1] ]
        down = cycle [ checkTile s x0 (cmod (y0 - y1) (height s)) 'v' | y1 <- [0..(height s)-1] ]

parse :: [String] -> Map
parse s = [ [ parseTile s x y | x <- [0..(width s)-1] ] | y <- [0..(height s)-1] ]

aoc :: [String] -> String
aoc f = show (goal+1)
  where trim = tail (init (map tail (map init f)))
        m = parse trim
        start = head [ i | i <- [1..], (getMap m (0,0)) !! i == 0 ]
        iter = iterate (bfs m) (start,[(0,0)])
        (goal,_) = last (takeWhile (\(cost,ps) -> not (elem ((width m)-1,(height m)-1) ps)) iter)
