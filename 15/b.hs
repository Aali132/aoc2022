module Aoc where

import Data.List

import Debug.Trace

type Point = (Int,Int)
type Sensor = (Point,Point)

manhattan :: Point -> Point -> Int
manhattan (xa,ya) (xb,yb) = abs (xa-xb) + abs (ya-yb)

checkPos :: [Sensor] -> Point -> Bool
checkPos s (x,y) = or [ (manhattan sp (x,y)) <= manhattan sp bp | (sp,bp) <- s ]

drawLine :: (Point,Point) -> [Point]
drawLine ((x0,y0),(x1,y1)) = zip [(min x0 x1)..(max x0 x1)] [(min y0 y1)..(max y0 y1)]

corners :: Sensor -> [Point]
corners ((xs,ys),(xb,yb)) = [ (xs + dist,ys),(xs - dist,ys),(xs,ys+dist),(xs,ys-dist) ]
  where dist = manhattan (xs,ys) (xb,yb) + 1

periphery :: Sensor -> [Point]
periphery s = concat (map drawLine (zip cs (tail cs)))
  where cs = corners s

parseLine :: String -> Sensor
parseLine s = ((xs,ys),(xb,yb))
  where xs = read (drop 2 (init ((words s) !! 2)))
        ys = read (drop 2 (init ((words s) !! 3)))
        xb = read (drop 2 (init ((words s) !! 8)))
        yb = read (drop 2 ((words s) !! 9))

frequency :: Maybe (Point,Bool) -> Int
frequency (Just ((x,y),_)) = x * 4000000 + y

aoc :: [String] -> String
aoc f = show (frequency (find snd [ (p, not (checkPos sensors p)) | p <- (filter (\(x,y) -> x <= maxX && x > 0 && y <= maxY && y > 0) (concat (map periphery sensors))) ] ))
  where sensors = map parseLine f
        maxX = if length sensors > 14 then 4000000 else 20
        maxY = if length sensors > 14 then 4000000 else 20
