module Aoc where

import Data.List.Split
import Data.List.Index

type Point = (Int,Int,Int)
type Map = [[[Int]]]

getMap :: Map -> Point -> Int
getMap m (x,y,z) = ((m !! z) !! y) !! x

setMap :: Int -> Point -> Map -> Map
setMap v (x,y,z) m = setAt z (setAt y (setAt x v ((m !! z) !! y)) (m !! z)) m

incMap :: Point -> Map -> Map
incMap p m = setMap ((getMap m p)+1) p m

sides :: Point -> [Point]
sides (x,y,z) = [ (x+1,y,z),(x-1,y,z),(x,y+1,z),(x,y-1,z),(x,y,z+1),(x,y,z-1) ]

openPoints :: Point -> Map -> Map
openPoints p m = foldr incMap m (sides p)

closePoints :: Map -> [Point] -> Map
closePoints m ps = foldr (setMap 0) m ps

parse :: String -> Point
parse s = (x+5,y+5,z+5)
  where [x,y,z] = Prelude.map read (splitOn "," s)

aoc :: [String] -> String
aoc f = show (sum ((concat.concat) mc))
  where points = map parse f
        m0 = take 30 (repeat (take 30 (repeat (take 30 (repeat (0))))))
        mo = foldr openPoints m0 points
        mc = closePoints mo points
