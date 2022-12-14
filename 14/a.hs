module Aoc where

import Data.List.Split
import Data.List.Index

data Tile = Air | Sand | Rock deriving Eq

type Map = [[Tile]]
type Point = (Int,Int)

setMap :: Tile -> Map -> Point -> Map
setMap v m (x,y) = setAt y (setAt x v (m !! y)) m

getMap :: Map -> Point -> Tile
getMap m (x,y) = (m !! y) !! x

drawLine :: (Point,Point) -> [Point]
drawLine ((x0,y0),(x1,y1))
  | x0 > x1 = zip [x1..x0] (repeat y0)
  | x0 < x1 = zip [x0..x1] (repeat y0)
  | y0 > y1 = zip (repeat x0) [y1..y0]
  | y0 < y1 = zip (repeat x0) [y0..y1]

mapLine :: Map -> [Point] -> Map
mapLine m ps = foldl (setMap Rock) m (concat (map drawLine (zip ps (tail ps))))

mapLines :: [[Point]] -> Int -> Map
mapLines ps ym = foldl mapLine (take (ym+1) (repeat (take 1000 (repeat Air)))) ps

parsePoint :: String -> Point
parsePoint s = (read (head xy), read (last xy))
  where xy = splitOn "," s

parseLine :: String -> [Point]
parseLine s = map parsePoint (splitOn " -> " s)

maxY :: [[Point]] -> Int
maxY ps = (maximum (map snd (concat ps)))

fallingSand :: Map -> Int -> Point -> Maybe Point
fallingSand m ym (x,y)
  | y >= ym = Nothing
  | getMap m (x,y+1) == Air = fallingSand m ym (x,y+1)
  | getMap m (x-1,y+1) == Air = fallingSand m ym (x-1,y+1)
  | getMap m (x+1,y+1) == Air = fallingSand m ym (x+1,y+1)
  | otherwise = Just (x,y)

foldingSand :: Int -> (Map,Bool) -> Point -> (Map,Bool)
foldingSand ym (m,stop) p = case fallingSand m ym p of
  Just r -> (setMap Sand m r,False)
  Nothing -> (m,True)

aoc :: [String] -> String
aoc f = show (fst (last (takeWhile (not.snd.snd) (zip [0..] (scanl (foldingSand ym) (m,False) (repeat (500,0)))))))
  where points = map parseLine f
        m = mapLines points ym
        ym = maxY points
