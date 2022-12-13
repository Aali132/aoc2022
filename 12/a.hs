module Aoc where

import Data.Char
import Data.List.Index
import Data.Maybe
import Data.List

type Map = [[Int]]
type Pos = (Int,Int)

width :: Map -> Int
width m = length (m !! 0)

height :: Map -> Int
height m = length m

parse :: [String] -> Map
parse s = (map.map) (\x -> (ord x) - (ord 'a')) s

setMap :: Int -> Map -> Pos -> Map
setMap v m (x,y) = setAt y (setAt x v (m !! y)) m

findStartEnd :: Map -> (Pos, Pos, Map)
findStartEnd m = (start, end, setMap 25 (setMap 0 m start) end)
  where start = head [ (x,y) | (y,line) <- zip [0..] m, x <- elemIndices (-14) line ]
        end = head [ (x,y) | (y,line) <- zip [0..] m, x <- elemIndices (-28) line ]

getMap :: Map -> Pos -> Int
getMap m (x,y) = (m !! y) !! x

possibleMoves :: (Map,Map) -> Pos -> [Pos]
possibleMoves (m,v) (x,y) = [ (xm,ym) | (xm,ym) <- [(x+1,y),(x-1,y),(x,y+1),(x,y-1)], xm < (width m), xm >= 0, ym < (height m), ym >= 0, ((getMap m (xm,ym)) - h) <= 1, (getMap v (xm,ym)) == -1 ]
  where h = getMap m (x,y)

lowestCost :: (Map,Map) -> [Pos] -> Pos -> Int -> Int
lowestCost (m,v) [] e cost = 999999
lowestCost (m,v) s e cost = if getMap v e > -1 then cost else lowestCost (m,foldl (setMap cost) v newPos) newPos e (cost+1)
  where newPos = nub (concat (map (possibleMoves (m,v)) s))

aoc :: [String] -> String
aoc f = show (lowestCost (map, repeat (repeat (-1))) [start] end 0)
  where (start, end, map) = findStartEnd (parse f)
