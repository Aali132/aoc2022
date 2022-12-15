module Aoc where

import Data.List

type Point = (Int,Int)
type Sensor = (Point,Point)

manhattan :: Point -> Point -> Int
manhattan (xa,ya) (xb,yb) = abs (xa-xb) + abs (ya-yb)

checkPos :: [Sensor] -> Int -> Int -> Bool
checkPos s x y = (or [ (manhattan sp (x,y)) <= manhattan sp bp | (sp,bp) <- s ]) && ((find (\(_,(bx,by)) -> bx == x && by == y) s) == Nothing)

parseLine :: String -> Sensor
parseLine s = ((xs,ys),(xb,yb))
  where xs = read (drop 2 (init ((words s) !! 2)))
        ys = read (drop 2 (init ((words s) !! 3)))
        xb = read (drop 2 (init ((words s) !! 8)))
        yb = read (drop 2 ((words s) !! 9))

aoc :: [String] -> String
aoc f = show (sum [ fromEnum (checkPos sensors x y) | x <- [minX..maxX] ])
  where sensors = map parseLine f
        maxX = maximum [ sx + manhattan (sx,sy) (bx,by) | ((sx,sy),(bx,by)) <- sensors ]
        minX = minimum [ sx - manhattan (sx,sy) (bx,by) | ((sx,sy),(bx,by)) <- sensors ]
        y = if length sensors > 14 then 2000000 else 10
