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
expand m t (x0,y0)
  | (x0,y0) == startwait = if (getMap m startp) !! t == 0 then [startwait,startp] else [startwait]
  | (x0,y0) == endwait = if (getMap m endp) !! t == 0 then [endwait,endp] else [endwait]
  | otherwise = [ p | (x1,y1) <- [(1,0),(-1,0),(0,1),(0,-1),(0,0)], x0+x1 >= 0, x0+x1 < (width m), y0+y1 >= 0, y0+y1 < (height m), let p = (x0+x1,y0+y1), (getMap m p) !! t == 0 ]
  where startwait = (0,-1)
        startp = (0,0)
        endwait = ((width m)-1,height m)
        endp = ((width m)-1,(height m)-1)

bfs :: Map -> (Int, [Pos]) -> (Int, [Pos])
bfs m (t,[]) = error "No valid path"
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
aoc f = show (goal3+1)
  where trim = tail (init (map tail (map init f)))
        m = parse trim
        startwait = (0,-1)
        startp = (0,0)
        endwait = ((width m)-1,height m)
        endp = ((width m)-1,(height m)-1)
        iter1 = iterate (bfs m) (0,[startwait])
        (goal1,_) = last (takeWhile (\(cost,ps) -> not (elem endp ps)) iter1)
        iter2 = iterate (bfs m) (goal1+1,[endwait])
        (goal2,_) = last (takeWhile (\(cost,ps) -> not (elem startp ps)) iter2)
        iter3 = iterate (bfs m) (goal2+1,[startwait])
        (goal3,_) = last (takeWhile (\(cost,ps) -> not (elem endp ps)) iter3)
